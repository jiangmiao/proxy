package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"sync/atomic"
	"time"
)

var (
	PASSWORD  = []byte("abcd1234")
	MASK      = byte(0x66)
	POOL_SIZE = 10
	VERSION   = "0.1.1"
	VERBOSE   = false
)

const (
	NORMAL = iota
	FAILED
)

func dd(args ...interface{}) {
	if VERBOSE {
		log.Println(args...)
	}
}

func flip(src []byte) []byte {
	data := make([]byte, len(src))
	for i, c := range src {
		data[i] = c ^ MASK
	}
	return data
}

type Stream struct {
	Id int64
	net.Conn
	Target *Stream
	Closed chan struct{}
}

var StreamId int64

func NewStream(conn net.Conn) *Stream {
	id := atomic.AddInt64(&StreamId, 1)
	stream := &Stream{
		Id:     id,
		Conn:   conn,
		Target: nil,
		Closed: make(chan struct{}),
	}
	return stream
}

func (s *Stream) FlipRead(n int) []byte {
	return flip(s.Read(n))
}

func (s *Stream) FlipWrite(src []byte) {
	s.Write(flip(src))
}

func (s *Stream) Read(n int) []byte {
	data := make([]byte, n)
	n, e := io.ReadFull(s.Conn, data)
	if e != nil {
		panic(e)
	}
	return data[:n]
}

func (s *Stream) Write(src []byte) {
	_, e := s.Conn.Write(src)
	if e != nil {
		panic(e)
	}
}

func (from *Stream) FlipPipe() {
	buf := make([]byte, 10240)
	for {
		n, e := from.Conn.Read(buf)
		if e != nil {
			break
		}
		_, e = from.Target.Conn.Write(flip(buf[:n]))
		if e != nil {
			break
		}
	}
	if from.Target != nil {
		from.Target.Close()
	}
	close(from.Closed)
}

// read and save to buffer
type ShadowStream struct {
	*Stream
	Buffer []byte
}

func (s *ShadowStream) Read(n int) []byte {
	data := s.Stream.Read(n)
	s.Buffer = append(s.Buffer, data...)
	return data
}

type Pool struct {
	RemoteAddr string
	Remotes    chan *Stream
	RemotesPre chan struct{}
	status     int64
}

func NewPool(remoteAddr string) *Pool {
	return &Pool{
		RemoteAddr: remoteAddr,
		Remotes:    make(chan *Stream, POOL_SIZE),
		RemotesPre: make(chan struct{}, POOL_SIZE),
		status:     FAILED,
	}
}

func (pool *Pool) CreateConnection() (*Stream, error) {
	conn, e := net.DialTimeout("tcp", pool.RemoteAddr, 15*time.Second)
	if e != nil {
		return nil, e
	}
	_, e = conn.Write(flip(PASSWORD))
	if e != nil {
		return nil, e
	}
	stream := NewStream(conn)
	go stream.FlipPipe()
	return stream, nil
}

func (pool *Pool) Fill() {
	for i := 0; i < 10; i++ {
		go func(i int) {
			for {
				if pool.status == FAILED && i > 0 {
					time.Sleep(500 * time.Millisecond)
					continue
				}
				pool.RemotesPre <- struct{}{}
				stream, e := pool.CreateConnection()
				if e != nil {
					log.Println("连接后端失败，等待1秒", e)
					pool.status = FAILED
					time.Sleep(1 * time.Second)
					<-pool.RemotesPre
					continue
				}
				pool.status = NORMAL
				pool.Remotes <- stream
				dd("预连接数", pool.Size())
			}
		}(i)
	}
}

func (pool *Pool) Size() int {
	return len(pool.Remotes)
}

func (pool *Pool) Get() (remote *Stream, e error) {
	if pool.status == FAILED {
		return nil, errors.New("连接后端失败")
	}
Retry:
	select {
	case remote = <-pool.Remotes:
		<-pool.RemotesPre
	default:
		remote, e = pool.CreateConnection()
		return
	}

	dd("使用", pool.Size())
	select {
	case <-remote.Closed:
		dd("连接已关闭，密码错或网络异常。")
		goto Retry
	default:
	}
	return
}

func Frontend(listenAddr string, remoteAddr string) {
	pool := NewPool(remoteAddr)
	go pool.Fill()

	// Listen
	l, e := net.Listen("tcp", listenAddr)
	if e != nil {
		log.Fatal(e)
	}
	log.Println("Frontend:", listenAddr)
	log.Println("UseBackend:", remoteAddr)
	log.Println("连接池大小:", POOL_SIZE)

	for {
		conn, e := l.Accept()
		if e != nil {
			log.Fatal(e)
		}
		go func() {
			defer func() {
				e := recover()
				if e != nil {
					log.Println(e)
				}
			}()
			defer conn.Close()

			client := NewStream(conn)

			r := client.Read
			w := client.Write

			var v []byte
			// SOCKS5
			r(1)            // VER
			v = r(1)        // NMETHOD
			r(int(v[0]))    // METHODS
			w([]byte{5, 0}) // VER METHOD
			r(3)            // VER CMD RSV

			// ATYP与之后的头要全部传到后台
			ss := &ShadowStream{client, make([]byte, 0, 256)}

			r = ss.Read
			v = r(1) // ATYP
			switch v[0] {
			case 1: // IP
				r(4)
			case 3: // DOMAIN
				v = r(1)     // LENGTH
				r(int(v[0])) // DOMAIN
			default:
				panic("Invalid ATYP")
			}
			r(2) // PORT

			var remote *Stream
			remote, e = pool.Get()
			if e != nil {
				return
			}

			// Reply immediately
			conn.Write([]byte{
				5, 0, 0, 1, // VER REP RSV ATYP
				0, 0, 0, 0, // BIND.ADDR
				0, 0, // BIND.PORT
			})

			remote.FlipWrite(ss.Buffer)
			remote.Target = client
			client.Target = remote
			go client.FlipPipe()
			<-client.Closed
			<-remote.Closed
		}()
	}
}

func Backend(listenAddr string) {
	l, e := net.Listen("tcp", listenAddr)
	if e != nil {
		log.Fatal(e)
	}

	log.Println("Backend:", listenAddr)
	for {
		conn, e := l.Accept()
		if e != nil {
			log.Fatal(e)
		}
		go func() {
			defer func() {
				e := recover()
				if e != nil {
					log.Println(e)
				}
			}()
			defer conn.Close()

			front := NewStream(conn)
			r := front.FlipRead

			// 密码认证
			front.SetDeadline(time.Now().Add(5 * time.Second))
			password := r(len(PASSWORD))
			if bytes.Compare(password, PASSWORD) != 0 {
				panic("Password does not match.")
			}
			front.SetDeadline(time.Time{})

			// ATypo
			d := r(1)
			var host string
			switch d[0] {
			case 1: // IP
				d := r(4)
				host = fmt.Sprintf("%d.%d.%d.%d", d[0], d[1], d[2], d[3])
			case 3: // DOMAIN
				d := r(1)
				host = string(r(int(d[0])))
			default:
				panic("Invalid ATYP")
			}
			d = r(2) // PORT
			port := int(d[0])<<8 + int(d[1])
			address := fmt.Sprintf("%s:%d", host, port)
			dd("访问地址", address)
			remoteConn, e := net.DialTimeout("tcp", address, 15*time.Second)
			if e != nil {
				panic(errors.New("Connect failed - " + e.Error()))
			}
			defer remoteConn.Close()
			remote := NewStream(remoteConn)

			front.Target = remote
			remote.Target = front
			go front.FlipPipe()
			go remote.FlipPipe()
			<-front.Closed
			<-remote.Closed
		}()
	}
}

func DisplayHelp() {
	fmt.Printf(`proxy version %s
用法： proxy [选项...] 方法

选项：
    -password=abcd1234 密码
    -poolsize=10       连接池大小
    -verbose

方法：
    front 后端地址 [前端监听地址] // 启动前端
    back 后端监听地址             // 启动后端
    test                          // 本地测试

备注：
    前后端地址格式同Go tcp包中Dial参数

本地测试：
    $ proxy -verbose test
    2015/02/11 00:28:20 Backend: 127.0.0.1:8781
    2015/02/11 00:28:20 Frontend: 127.0.0.1:8780
    2015/02/11 00:28:20 UseBackend: 127.0.0.1:8781
    2015/02/11 00:28:20 连接池大小: 10
    2015/02/11 00:28:20 预连接数 1
    2015/02/11 00:28:20 预连接数 2
    2015/02/11 00:28:20 预连接数 3
    2015/02/11 00:28:20 预连接数 4
    ...

    $ curl -v --socks5-hostname 127.0.0.1:8780 http://baidu.com

远程实例：
    # 在代理服务器(IP 1.2.3.4)中运行
    $ proxy back :8781

    # 在本机运行，默认监听 127.0.0.1:8780
    $ proxy front 1.2.3.4:8781

    # 测试
    $ curl -v --socks5-hostname 127.0.0.1:8780 http://baidu.com

    # 或指定本机地址
    $ proxy front 1.2.3.4:8781 127.0.0.1:8782

    # 带参数的命令
    $ proxy -verbose -poolsize=5 -password=Password test
`, VERSION)
}

func main() {
	frontAddr := "127.0.0.1:8780"
	backAddr := "127.0.0.1:8781"

	passwordPtr := flag.String("password", "abcd1234", "密码")
	poolSizePtr := flag.Int("poolsize", 10, "连接池大小")
	verbosePtr := flag.Bool("verbose", false, "显示调试信息")
	flag.Parse()

	n := flag.NArg()
	if n == 0 {
		DisplayHelp()
		return
	}

	PASSWORD = []byte(*passwordPtr)
	VERBOSE = *verbosePtr
	POOL_SIZE = *poolSizePtr

	args := flag.Args()
	switch args[0] {
	case "front":
		if n > 1 {
			backAddr = args[1]
		}
		if n > 2 {
			frontAddr = args[2]
		}
		Frontend(frontAddr, backAddr)
	case "back":
		if n > 1 {
			backAddr = args[1]
		}
		Backend(backAddr)
	case "test":
		go Frontend(frontAddr, backAddr)
		Backend(backAddr)
	default:
		backAddr = args[0]
		if n > 1 {
			frontAddr = args[1]
		}
		Frontend(frontAddr, backAddr)
	}
}
