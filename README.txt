支持SOCKS 5协议的高速加密通信的代理服务器脚本

代码地址：

    https://github.com/jiangmiao/proxy

解决现有代理的问题：

    1、vpn        - vpn接管所有的数据，而在很多时更希望只通过代理访问部份网站。
    2、http/socks - 仅使用http或socks代理，没有经过加密的关键词还是有被拦截的可能。
    3、ssh+socks  - ssh tunnel的socks性能与稳定性不佳。
    4、网页proxy  - 兼容性较差。

系统需求：

    Erlang R15B02 测试通过。

使用说明：

    服务端：erlc proxy.erl && erl -noshell -s proxy back_start 0.0.0.0 8781
            back_start 带1个参数 监听端口 [监听地址: 0.0.0.0]
                       带2个参数 监听地址 监听端口

    客户端：erlc proxy.erl && erl -noshell -s proxy front_start 服务端IP或域名 8781 127.0.0.1 8780 
            front_start 带1个参数 服务端IP或域名 [8781 127.0.0.1 8780]
                        带2个参数 服务端IP或域名 服务器端口 [127.0.0.1 8780]
                        带4个参数 服务端IP或域名 服务器端口 监听地址 监听端口

    通过 proxy.sh:

        其中 erlc proxy.erl && erl -noshell -s proxy 可以通过 proxy.sh 代替
        如 
            proxy.sh back_start 8781 开启监听 0.0.0.0:8781 的Back端
            proxy.sh front_start 1.2.3.4 监听 127.0.0.1:8780 并连接 1.2.3.4:8781 的Back端

本地测试：

    # 获取代码
    $ git clone https://github.com/jiangmiao/proxy
    正克隆到 'proxy'...
    remote: Counting objects: 15, done.
    remote: Compressing objects: 100% (11/11), done.
    remote: Total 15 (delta 2), reused 15 (delta 2)
    Unpacking objects: 100% (15/15), done.

    $ cd proxy

    $ erlc proxy.erl && erl -noshell -s proxy start
    或
    $ ./proxy.sh start
    back listen at 0.0.0.0:8781.
    front listen at 127.0.0.1:8780.
    back address is 127.0.0.1:8781.

    $ curl -I --socks5 127.0.0.1:8780 www.baidu.com
    HTTP/1.1 200 OK
    Date: Wed, 05 Dec 2012 16:43:08 GMT
    Server: BWS/1.0
    Content-Length: 9777
    ...

工作原理：

    Client - 客户端：如浏览器
    Front  - 代理前端：从 客户端 接收符合Socks5协议的数据，简单加密后转发给 后端，
                       从 后端 接收数据解密后转发给 客户端。
    Back   - 代理后端：从 前端 接收数据，并作为Socks5代理服务器处理数据，转发给 远程服务器。
                       从 远程服务器 接收数据，并密转发给 前端 。
    Remote - 远程服务器：需要请求的目标服务器

    Client 与 Front  为本地通信
    Back   与 Remote 为异地原文转发
    Front  与 Back   为异地加密通信

加密算法：

    Front 与 Back 之间的通信加密算法为 每个字节 异或 0x66 (01100110)

Front 与 Back 的认证协议：

    Front 发送 "abcd1234"
    Back  没有断开表示认证通过。

    认证密码 abcd1234 可以在 proxy.erl 中修改

性能与稳定性：

    由于使用了Erlang，性能和稳定性没得说。

    对于SOCKS的协议略微有更改，握手部份由Front完成，且始终返回成功。每一个独立请求由3回数据交换（Socks 2次认证 + 1次原请求）变成了1次，经测试对于250ms ping值的服务器每个独立请求比传统SOCKS请求可以节约0.5~0.7秒的时间。

    采用连接池，免去客户端与代理服务器的连接时间，对于250ms的服务器缩短请求时间0.3秒左右时间。

功能局限：

    对socks5的协议支持并不完善，比如不支持BIND，IPV6。但在大多数情况下，比如浏览器，tsocks是没有问题的。

LICENCE：

    MIT
