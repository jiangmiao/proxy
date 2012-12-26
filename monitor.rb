# encoding: utf-8

require 'socket'
require 'btk'
include Gtk

INFO_ID = 0
INFO_ADDRESS = 1
INFO_ACTIVED_AT = 2
INFO_FIELDS_NUM = 3

COLS = ["序号", "地址", "活跃时间"]
COLS_NUM = COLS.size

mutex = Mutex.new

$infos_hash = {}
$sel = nil

FRONT_PORT = $ARGV[0] || 8780

# \x99, \x00
def get_infos
	s = nil
	s = TCPSocket.new 'localhost', FRONT_PORT
	s.send("\x99\x00", 0)

	length = s.recv(4).unpack("i>")[0]
	raw_infos = s.recv(length).split(",")
	infos = []
	i = 0
	while true
		info = raw_infos[i..i+3]
		i += INFO_FIELDS_NUM
		break unless info[INFO_ID]
		next if info[INFO_ADDRESS] == ""
		info[INFO_ID] = info[INFO_ID].to_i
		infos << info
	end

	infos.sort! {|a, b|
		a[INFO_ID] <=> b[INFO_ID]
	}

	now = Time.now.to_i
	infos.map! {|info|
		actived_at = (info[INFO_ACTIVED_AT].to_i/1e6).to_i
		diff = now - actived_at
		if diff < 60
			actived_at_str = "#{diff} 秒前"
		elsif diff < 3600
			actived_at_str = "#{diff/60} 分钟前"
		elsif
			actived_at_str = Time.at(actived_at).strftime("%T")
		end
		info[INFO_ACTIVED_AT] = actived_at_str
		info[INFO_ID] = info[INFO_ID].to_s
		info
	}
	infos
ensure
	s.close() if s
end

# \x98, \x00, PortNum:16, PortId:16 * PortNum
def close_ports(ids)
	s = nil
	data = ([ids.length] + ids).pack("S>*")
	s = TCPSocket.new 'localhost', FRONT_PORT
	s.send("\x98\x00", 0)
	s.send(data, data.bytesize)
ensure
	s.close() if s
end


Btk.Window :default_size=>[300, 300], :window_position=>Window::POS_CENTER do|w|
	w.sig_destroy do
		puts "done."
		Gtk.main_quit
	end

	w.VBox do|vbox|
		vbox.ScrolledWindow :policy=>[POLICY_AUTOMATIC, POLICY_AUTOMATIC] do|sw|
			store = ListStore.new String, String, String, String
			$store = store
			renderer = CellRendererText.new
			sw.TreeView store do|tv|
				COLS.each_index do|idx|
					col = TreeViewColumn.new(COLS[idx], renderer, :text => idx)
					col.resizable=true
					tv.append_column(col)
				end

				tv.selection.mode=SELECTION_MULTIPLE
				$sel = tv.selection
			end

			Thread.new do
				iters = {}
				while true
					begin
						mutex.synchronize do
							$infos_hash = {}
							infos = get_infos()
							remove_list = Hash[iters.keys.zip]
							infos.each {|info|
								id = info[INFO_ID]
								remove_list.delete(id)
								iter = iters[id]
								unless iter
									iter = store.prepend
									iters[id] = iter
								end

								COLS.each_index do |idx|
									iter[idx] = info[idx]
								end
								$infos_hash[id] = info
							}
							for id in remove_list.keys
								store.remove(iters[id])
								iters.delete(id)
							end
						end
						sleep 1
					rescue
						puts "#{$!.message.force_encoding("utf-8")}, 等待2秒"
						# puts $@
						sleep 2
					end
				end
			end
		end

		vbox.HBox :pack=>false do|hbox|
			hbox.Button "中断连接", :pack=>false do|btn|
				btn.signal_connect 'clicked' do
					mutex.synchronize do
						ids = $sel.selected_rows.collect do|path|
							iter = $store.get_iter(path)
							id = iter[0]
							$infos_hash[id][INFO_ID].to_i
						end
						close_ports(ids)
					end
				end
			end
		end
	end

	w.show_all
end

Gtk.main
