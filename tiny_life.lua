
-- title: Tiny Life
-- author: MetalDudeBro
-- desc: A toy Game of Life
-- script: lua
-- input: mouse, keyboard
-- saveid: TinyLife
-- version 1.3
--=--=--=--=--=--=--=--=--=--=--=--=--
-- MIT License (c) 2019 Skaruts (MetalDudeBro)
--=--=--=--=--=--=--=--=--=--=--=--=--
-- RESEARCH:
--    http://www.mirekw.com/ca/ca_files_formats.html
--    https://www.ibiblio.org/lifepatterns/
--
--    fast life algorithms:
--        https://stackoverflow.com/questions/3554552/game-of-life-memory-optimization
--            - https://stackoverflow.com/a/6164868
--            - https://www.ibiblio.org/lifepatterns/lifeapplet.html
--=--=--=--=--=--=--=--=--=--=--=--=--
-- utils & shortenings
	local ins,sort,rem,conc,unpk=table.insert,table.sort,table.remove,table.concat,table.unpack
	local floor,ceil,max,min,abs,rand,sqrt=math.floor,math.ceil,math.max,math.min,math.abs,math.random,math.sqrt
	local assert,tonum,tostr,type,setmt,getmt,pairs,ipairs=assert,tonumber,tostring,type,setmetatable,getmetatable,pairs,ipairs
	local gsub,fmt,rep=string.gsub,string.format,string.rep

	local keys={
		PGUP=54,PGDN=55,SPACE=48,ENTER=50,BSLASH=41,GRAVE=44,TAB=49,
		A=1,B=2,C=3,D=4,E=5,F=6,G=7,H=8,I=9,J=10,K=11,L=12,M=13,N=14,O=15,P=16,
		Q=17,R=18,S=19,T=20,U=21,V=22,W=23,X=24,Y=25,Z=26,
		N0=27,N1=28,N2=29,N3=30,N4=31,N5=32,N6=33,N7=34,N8=35,N9=36,
		CTRL=63,SHFT=64,ALT=65,
		UP=58,DOWN=59,LEFT=60,RIGHT=61,
	}
	--[[ Debug drawing stuff                   003 ]]
		-- TODO: add a grid-step choice
		local function draw_dbg_grid(c)
			c=c or 12
			for i=0,240,8 do line(i,0,i,136,c)end
			for j=0,136,8 do line(0,j,240,j,c)end
		end
		local function draw_dbg_center_lines(c)
			c=c or 15
			line(240/2,0,240/2,136,c)
			line(0,136/2,240,136/2,c)
		end

	--[[ tm_check - time stuff / dt            003 ]]
		-- call 'tsecs' without args to get current time in secs
		-- call 'tm_check' at the start of every frame to update t/dt/t2
		local f,t1,t2,dt,tsecs,tm_check=0,time()
		function tsecs(ms) return (ms or time())/1000 end
		function tm_check()
			f=f+1
			t2=time()
			dt=(t2-t1)/1000
			t1=t2
		end
	--[[ has - check if 'o' is in table 't'    001 ]] local function has(t,o) for i=1,#t do if t[i]==o then return true end end end
	--[[ dmerge - merge dict/hash tables       001 ]] local function dmerge(a,b,err,...) local has,type=has,type err=err or"Key '%s' already exists in table."local t={} for k,v in pairs(a)do if type(k)~="number"then t[k]=v end end for k,v in pairs(b)do if type(k)~="number" then if has(t,k)then print(fmt(err,...or k)) else t[k]=v end end end return t end

	--[[ clamp - keep v between l and h        002 ]] local function clamp(v,l,h)return max(l,min(v,h))end
	--[[ wrap - wrap v around l and h          003 ]] local function wrap(v,l,h) return v > h and l or v<l and h or v end
	--[[ round - round v to nearest int        003 ]] local function round(v)return floor(v+0.5)end
	--[[ lerp - linear interpolate             002 ]] local function lerp(a,b,t)return a*(1-t)+b*t end

	--[[ txtw - get text width                 002 ]] local function txtw(tx,fw,s,sf) return print(tx,0,-99,-1,fw or false,s or 1,sf or false) end
	--[[ prints - print with shadow            003 ]]
		local function prints(tx,x,y,c,sc,fw,s,sf)
			fw,s,sc,sf=fw or false,s or 1,sc or 1,sf or false
			print(tx,x+1,y+1,sc,fw,s,sf)
			print(tx,x,y,c,fw,s,sf)
		end
	--[[ printgs - print on grid with shadow   009 ]]
		local function printgs(t,x,y,c,sc,fw,s,ox,oy)
			fw,s,sc=fw or false,s or 1,sc or 1
			x,y=x*8+1+(ox or 0),y*8+1+(oy or 0)
			print(t,x+1,y+1,sc,fw,s)
			print(t,x,y,c,fw,s)
		end
	--[[ printgsc - print grid/shadow/centered 010 ]] local function printgsc(tx,x,y,c,sc,fw,s) fw,s,sc=fw or false,s or 1,sc or 1 if not x then x=(240//8)//2-(txtw(tx)//8)//2 end if not y then y=(136//8)//2 end print(tx,x*8+1,y*8+1,sc,fw,s) print(tx,x*8,y*8,c,fw,s) end
	--[[ Print grid w/ drop shadow centered  (0.07) ]] local function printc(t,x,y,c,fix_w)local xc,yc,c,len=1,1,c or 15,print(t,-999,-999)x=x==nil and(240-len)//2or x*8 y=y==nil and 136//2-8//2or y*8 print(t,x+xc+1,y+yc+1,shad_c,fix_w)print(t,x+xc,y+yc,c,fix_w)end
	--[[ printgo - print grid/outline          001 ]]
		local function printgo(tx,x,y,c,oc)
			x,y=x*8,y*8
			for j=y-1,y+1 do
				for i=x-1,x+1 do
					print(tx,i,j,oc)
				end
			end
			print(tx,x,y,c)
		end
	--[[ printo - print with outline           003 ]]
		-- Use whichever version is preferable. The second one
		-- can be tweaked to only do diagonals or sides, making
		-- a thinner outline.
		local function printo(tx,x,y,c,oc,fw,s,sf)
			fw,s,oc,sf=fw or false,s or 1,oc or 1,sf or false
			for j=y-1,y+1 do
				for i=x-1,x+1 do
					print(tx,i,j,oc,fw,s,sf)
				end
			end
			print(tx,x,y,c,fw,s,sf)
		end

	--[[ string.split - split string at 'sp'   001 ]]
		-- split string by delimiter 'sp'
		-- TODO: iirc this had some edge case issues
		local _DEF_PAT,_L_PAT,_R_PAT='([^, ]+)','([^',']+)'
		-- 'sp' is a string that lists separators to be used
		function string.split(s,sp)
			local t={}
			if sp=="" then
				for i=1,#s do
				    t[#t+1]=s:sub(i,i)
				end
			else
				sp=sp and _L_PAT..sp.._R_PAT or _DEF_PAT
				for word in s:gmatch(sp)do
				    t[#t+1]=word
				end
			end
			return t
		end

	--[[ tracef - trace formatted              001 ]] function tracef(...) trace(fmt(...)) end
	--[[ tracec - trace csv arguments          003 ]] function tracec(...) local t={} for i=1,select("#",...)do t[i]=tostr(select(i,...)) end trace(conc(t,",")) end

	--[[ trace1d - trace a 1D array            001 ]]
		local function trace1d(t)
			-- TODO: couldn't this just use table.concat, like 'trace2d' does?
			local s=""
			for i,v in ipairs(t)do
				s=s..v..","
			end
			trace(s)
		end
	--[[ Vector2 (stripped)             (0.02) ]]
		local _VECMT,vecv,vec0,vec2={}
		_VECMT={
			__index=_VECMT,
			__tostring=function(t)return fmt("(%s,%s)",t.x,t.y)end,
			__add=function(a,b)return type(b)=="number"and vec2(a.x+b,a.y+b)or vec2(a.x+b.x,a.y+b.y)end,
			__sub=function(a,b)return type(b)=="number"and vec2(a.x-b,a.y-b)or vec2(a.x-b.x,a.y-b.y)end,
			__mul=function(a,b)return type(b)=="number"and vec2(a.x*b,a.y*b)or vec2(a.x*b.x,a.y*b.y)end,
			__div=function(a,b)return type(b)=="number"and vec2(a.x/b,a.y/b)or vec2(a.x/b.x,a.y/b.y)end,
			__idiv=function(a,b)return type(b)=="number"and vec2(a.x//b,a.y//b)or vec2(a.x//b.x,a.y//b.y)end,
			__eq=function(a,b)return a.x==b.x and a.y==b.y end,
			abs=function(v)return vec2(abs(v.x),abs(v.y))end,
		}
		function vecv(v)return setmt({x=v.x or v[1],y=v.y or v[2]},_VECMT)end
		function vec0()return setmt({x=0,y=0},_VECMT)end
		function vec2(x,y)return setmt({x=x,y=y},_VECMT)end
	--[[ rec - a rectangle object              004 ]]
		local _RMT,_REC_RO,_RILUT,_RNILUT,rec0,recr,recv,rec2={}
		function rec0()return setmt({x=0,y=0,w=0,h=0},_RMT)end
		function recr(r)return setmt({x=r.x or r[1],y=r.y or r[2],w=r.w or r[3],h=r.h or r[4]},_RMT)end
		function rec2(a,b)return setmt({x=a.x or a[1],y=a.y or a[2],w=b.x or b[1],h=b.y or b[2]},_RMT)end
		function rec4(x,y,w,h)return setmt({x=x,y=y,w=w,h=h},_RMT)end
		_REC_RO={c=true,tl=true,br=true}
		_RILUT={
			x2=function(t,k)return t.x+t.w end,
			y2=function(t,k)return t.y+t.h end,
			c=function(t,k)return vec2((t.x+t.x+t.w)//2,(t.y+t.y+t.h)//2)end,
			tl=function(t,k)return vec2(t.x,t.y)end,
			br=function(t,k)return vec2(t.x+t.w,t.y+t.h)end,
			p=function(t,k)return vec2(t.x,t.y)end,
			s=function(t,k)return vec2(t.w,t.h)end,
		}
		_RNILUT={
			x2=function(t,v)t.w=v-t.x end,
			y2=function(t,v)t.h=v-t.y end,
			p=function(t,v)t.x,t.y=v.x,v.y end,
			s=function(t,v)t.w,t.h=v.x,v.y end,
		}
		_RMT={
			__index=function(t,k)
				if _RMT[k]~=nil then return _RMT[k]end
				if _RILUT[k]then return _RILUT[k](t,k)end
				error(fmt("bad index '%s' for rect",tostr(k)),2)
			end,
			__newindex=function(t,k,v)
				if _REC_RO[k]then error(fmt("'%s' is read only",tostr(k)))end
				if _RNILUT[k]then return _RNILUT[k](t,v)end
				error(fmt("bad new index '%s' for rect",tostr(k)),2)
			end,
			__tostring=function(t)return fmt("(%s,%s,%s,%s)",t.x,t.y,t.w,t.h)end,
			__eq=function(t,o)return t.x==o.x and t.y==o.y and t.w==o.w and t.h==o.h end,

			sq=function(t)
				return rec4(
					t.x, t.y,
					t.w > t.h and t.h or t.w,
					t.w > t.h and t.h or t.w
				)
			end,
		}
	--[[ Bresenham Stuff                       004 ]]
		local Bres={}
		Bres={
			line=function(x1,y1,x2,y2,exclude_start)
				exclude_start=exclude_start or false
				local pts,dtx,dty={},x2-x1,y2-y1
				local ix,iy=dtx>0 and 1or-1,dty>0 and 1or-1
				dtx,dty=2*abs(dtx),2*abs(dty)
				if not exclude_start then
					pts[#pts+1]={x=x1,y=y1}
				end
				if dtx>=dty then
					err=dty-dtx/2
					while x1~=x2 do
						if err>0or(err==0 and ix>0)then
							err,y1=err-dtx,y1+iy
						end
						err,x1=err+dty,x1+ix
						pts[#pts+1]={x=x1,y=y1}
					end
				else
					err=dtx-dty/2
					while y1~=y2 do
						if err>0or(err==0 and iy>0)then
							err,x1=err-dty,x1+ix
						end
						err,y1=err+dtx,y1+iy
						pts[#pts+1]={x=x1,y=y1}
					end
				end
				return pts
			end,
			-- not working properly
			ellipse=function(xc,yc,w,h)
				if w<=0 or h<=0 then return{}end
				local pts,a2,b2,x,y,sig={},w*w,h*h,0,0,0
				local p=function(x,y)pts[#pts+1]={x=x,y=y}end
				local _put_pts=function(xc,yc,x,y)
					p(xc+x,yc+y)
					p(xc-x,yc+y)
					p(xc+x,yc-y)
					p(xc-x,yc-y)
				end

				local fa2,fb2=4*a2,4*b2

				-- first half
				x,y=0,h
				sig=2*b2+a2*(1-2*h)
				while b2*x<=a2*y do
					_put_pts(xc,yc,x,y)
					if sig>=0 then
						sig=sig+fa2*(1-y)
						y=y-1
					end
					sig=sig+b2*((4*x)+6)
					x=x+1
				end
				-- second half
				x,y=w,0
				sig=2*a2+b2*(1-2*w)
				while a2*y<=b2*x do
					_put_pts(xc,yc,x,y)
					if sig>=0 then
						sig=sig+fb2*(1-x)
						x=x-1
					end
					sig=sig+a2*((4*y)+6)
					y=y+1
				end
				return pts
			end,
			circle=function(xc,yc,r)
				-- TODO: test this
				local pts={}
				-- TODO: are inner functions like these bad?
				local p=function(x,y)pts[#pts+1]={x=x,y=y}end
				local circle=function(xc,yc,x,y)
					p(xc+x,yc+y)
					p(xc-x,yc+y)
					p(xc+x,yc-y)
					p(xc-x,yc-y)
					p(xc+y,yc+x)
					p(xc-y,yc+x)
					p(xc+y,yc-x)
					p(xc-y,yc-x)
				end

				local x,y,d=0,r,3-2*r

				circle(xc,yc,x,y)
				while y>=x do
					x=x+1
					if d>0 then
						y=y-1
						d=d+4*(x-y)+10
					else
						d=d+4*x+6
					end
					circle(xc,yc,x,y)
				end
				return pts
			end
		}
	--[[ Mouse States                          007 ]]
		local mx,my,mwx,mwy,lmx,lmy,rmx,rmy=0,0,0,0,0,0,0,0
		local M1,M2,M3,m1,m2,m3=1,2,3,false,false,false
		local m_stt={prev={0,0,0},curr={0,0,0}}
		local mbtn,mbtnp,mbtnr,mbtnt,update_mst
		function mbtn(b)
			if b then return m_stt.curr[b]>0 end
			return m_stt.curr[1]>0
			    or m_stt.curr[2]>0
			    or m_stt.curr[3]>0
		end
		function mbtnp(b)
			if b then return m_stt.curr[b]==1 end
			return m_stt.curr[1]==1
			    or m_stt.curr[2]==1
			    or m_stt.curr[3]==1
		end
		function mbtnr(b)
			if b then return m_stt.prev[b]>0 and m_stt.curr[b]==0 end
			return m_stt.prev[1]>0 and m_stt.curr[1]==0
			    or m_stt.prev[2]>0 and m_stt.curr[2]==0
			    or m_stt.prev[3]>0 and m_stt.curr[3]==0
		end
		function mbtnt(b)
			if b then return m_stt.curr[b] end
			return m_stt.curr[1],m_stt.curr[2],m_stt.curr[3]
		end
		function update_mst()
			lmx,lmy=mx,my
			mx,my,m1,m3,m2,mwx,mwy=mouse()
			-- attempt to fix negative mouse issue
			if mx==255 and lmx<120 then mx=-1 end
			if my==255 and lmy<68 then my=-1 end
			rmx,rmy=mx-lmx,my-lmy
			m_stt.prev={m_stt.curr[1],m_stt.curr[2],m_stt.curr[3]}
			m_stt.curr={0,0,0}
			if m1 then m_stt.curr[1]=m_stt.prev[1]+1 end
			if m2 then m_stt.curr[2]=m_stt.prev[2]+1 end
			if m3 then m_stt.curr[3]=m_stt.prev[3]+1 end
		end
		function mmoved()
			return mx~=lmx or my~=lmy
		end
	--[[ Debug/benchmark utilities             018 ]] local _f,monitor,bm,bma=1 local dbg={key=41, fg=6, bg=2, active=false, use_padding=true, fixw=true, h=0,w=0,vals=nil,reg={}, toggle=function(t)t.active=not t.active end, spaced=function(t,b)t.use_padding=b end, draw=function(t) _f=_f+1 if _f>500 then _f=1 for k,_ in pairs(t.reg)do t.reg[k]=0 end end if not t.active then return end if t.use_padding then local w=t.w*8-t.w*2 rect(0,0,w+8,t.h*8+8,t.bg) for i=1,#t.vals do print(t.vals[i],2,(i-1)*8+2,t.fg,t.fixw) end t.vals,t.w={},0 else local w=txtw(t.vals,t.fixw) rect(0,0,w+8,(t.h+1)*6,t.bg) print(t.vals,2,2,t.fg,t.fixw) t.vals=""end t.h=0 end, } dbg.vals=dbg.use_padding and{}or""function monitor(k,v,n) local t=dbg if not t.active then return end if t.use_padding then local s if v==nil then s=k elseif k~=""then if n then k=k..rep(' ',n-#k) end s=conc({k,tostr(v)}) else s=tostr(v) end t.vals[#t.vals+1]=s if #s>t.w then t.w=#s end else if v==nil then t.vals=conc({t.vals,k,'\n'}) elseif k~=""then if n then k=k..rep(' ',n-#k) end t.vals=conc({t.vals,k,tostr(v),'\n'}) else t.vals=conc({t.vals,tostr(v),'\n'}) end end t.h=t.h+1 end function bm(id,f) local tm=time() f() monitor(id, fmt("%.2fms",time()-tm)) end function bma(id,f) local rg,t1,t2,s=dbg.reg if not rg[id]then rg[id]=0 end t1=time() f() t2=time()-t1 s=fmt("%.2fms",t2) rg[id]=rg[id]+t2 s=s..rep(' ',9-#s)..fmt("%.2fms",rg[id]/_f) monitor(id..rep(' ',11-#id),s) end

--=--=--=--=--=--=--=--=--=--=--=--=--
	--[[ TICkle IMGUI                           011 ]]
		-- https://github.com/Skaruts/TIC-80-utils-and-libs/tree/main/TICkle
		-- depends on
		--     unpk, setmt, fmt  (common_shortenings.lua)
		--     mouse_states      (mouse/mouse_states.lua)
		--     dmerge            (table_utils.lua)
		--     clip/with_clip    (drawing_utils.lua)
		--
		-- TODO: find a way to detatch tooltip from ui, to allow user
		--       to implement it however he wants. Make it an extension?
		--       Problem is, the tooltip needs some time-keeping/updating...
		--
		--=--=--=--=--=--=--=--=--=--=--=--
		--       Base Stuff
			-- locked means frozen (e.g. when a modal popup takes over)
			-- inactive means frozen and faded

			local _NID,_NIT,_NOK="NO_ID","NO_ITEM","NO_KEY"
			local ui={
				visible=true,
				active=true,
				locked=false,
				mouse_on_ui=false,
				shift=false,
				ctrl=false,
				alt=false,
				info_item=_NIT,
				_rend_steps={},
				_addons={},
				_items={},
				_curr={hovd=_NID,prsd=_NID},
				_prev={hovd=_NID,prsd=_NID},
				_cache={i={},h={},p={}},
				_kb_focus=_NID,
				_key_entered=_NOK,
				_key_char=nil,
				_text_changed=false,
				_it_state_changed=false,
				_timer={
					on=false,
					t=0,
					elapsed=0,
					cooldown=0,
				},
			}

			function ui.add_addon(adn)ui._addons[#ui._addons+1]=adn end

			function ui._push(it)ui._items[#ui._items+1]=it end
			function ui._pop()ui._items[#ui._items]=nil end
			function ui._peek()return ui._items[#ui._items]end

		--=--=--=--=--=--=--=--=--=--=--=--
		--=--=--=--=--=--=--=--=--=--=--=--
		--       State stuff
			function ui.show()ui.visible=true end
			function ui.hide()ui.visible=false end
			function ui.with_visible(b,f,...)
				local prev,ret=ui.visible
				ui.visible=b
				ret=f(...)
				ui.visible=prev
				return ret
			end

			function ui.enable()ui.active=true end
			function ui.disable()ui.active=false end
			function ui.with_active(b,f,...)
				local prev,ret=ui.active
				ui.active=b
				ret=f(...)
				ui.active=prev
				return ret
			end

			function ui.lock()ui.locked=true end
			function ui.unlock()ui.locked=false end
			function ui.with_locked(b,f,...)
				local prev,ret=ui.locked
				ui.locked=b
				ret=f(...)
				ui.locked=prev
				return ret
			end
		--=--=--=--=--=--=--=--=--=--=--=--
		--=--=--=--=--=--=--=--=--=--=--=--
		--       Cache Stuff
			-- item cache - ids (current frame), hovered ids and pressed ids (last frame)
			function ui._cache_item(it)
				-- if ID is claimed, widget is back alive, remove ID (see 'ui.end_frame()')
				if ui._cache.i[it.id]then ui._cache.i[it.id]=nil end
			end

			function ui._is_cached_hovd(id)return ui._cache.h[id]~=nil end
			function ui._is_cached_prsd(id)return ui._cache.p[id]~=nil end

			function ui._cache_hovd(id)ui._cache.h[id]=true end
			function ui._cache_prsd(id)ui._cache.p[id]=true end

			function ui._uncache_hovd(id)ui._cache.h[id]=nil end
			function ui._uncache_prsd(id)ui._cache.p[id]=nil end

		--=--=--=--=--=--=--=--=--=--=--=--
		--=--=--=--=--=--=--=--=--=--=--=--
		--       Rendering Stuff
			function ui.push_render_step(name,args)
				ui._rend_steps[#ui._rend_steps+1]={name,args}
			end

			ui._rend_step_fns={
				rect=rect,
				rectb=rectb,
				line=line,
				print=print,
				spr=spr,
				clip=clip,
			}

			function ui.add_render_step(nm,rs,rsf)
				ui[nm]=rs
				ui._rend_step_fns[nm]=rsf
			end

			function ui.rect(...)ui.push_render_step("rect",{...})end
			function ui.rectb(...)ui.push_render_step("rectb",{...})end
			function ui.line(...)ui.push_render_step("line",{...})end
			function ui.print(...)ui.push_render_step("print",{...})end
			function ui.spr(...)ui.push_render_step("spr",{...})end
			function ui.clip(...)
				local b = {...}
				ui._peek().bounds=b
				ui.push_render_step("clip",b)
			end

			function ui.with_clip(x,y,w,h,f,...)
				ui.clip(x,y,w,h)
				f(...)
				ui.clip()
			end
		--=--=--=--=--=--=--=--=--=--=--=--
		--=--=--=--=--=--=--=--=--=--=--=--
		--       Loop Stuff
			function ui._run_addons(fname)
				for _,addon in ipairs(ui._addons) do
					local f=addon[fname]
					if f then f(addon,it) end
				end
			end

			ui._uchars,ui._chars=
				'ABCDEFGHIJKLMNOPQRSTUVWXYZ=!"#$%&/()_ «»?|;:',
				"abcdefghijklmnopqrstuvwxyz0123456789- <>\\'~`,.  "

			function ui._input(event)
				ui.shift=key(64)
				ui.ctrl=key(63)
				ui.alt=key(65)
				-- ui.tab=keyp(49)

				for i=1,62 do
					if keyp(i, 25, 5) then
						ui._key_entered=i

						-- TODO: problem: TIC's keycodes are incomplete and
						--       incompatible with foreign keyboards,
						--       so receiving text input is limited
						if i <= 48 then
							ui._key_char=(ui.shift and ui._uchars:sub(i,i) or ui._chars:sub(i,i))
						end
					end
				end
				-- monitor("key: ", ui._key_entered)
				-- monitor("char: ", ui._key_char)
				ui._run_addons("input")
			end

			function ui.start_frame()
				ui._input()

				ui.mouse_on_ui=false
				ui._timer.elapsed=ui._timer.elapsed+dt

				if ui._timer.count then
					ui._timer.t=ui._timer.t+dt
					ui._timer.cooldown=ui._timer.cooldown-dt
				end

				if not ui._cache.h[ui.info_item.id]then
					ui.info_item=_NIT
				end

				ui._run_addons("start_frame")
			end

			function ui._render()
				if ui.visible then
					local unpk,_rend_step_fns=unpk,ui._rend_step_fns
					for _,v in ipairs(ui._rend_steps)do
						_rend_step_fns[v[1]](unpk(v[2]))
					end
					ui._rend_steps={}
				end
			end

			function ui.end_frame()
				ui._run_addons("end_frame")

				ui._render()

				if m1 and(ui._curr.prsd==_NID or ui._curr.hovd==_NID)then
					ui._kb_focus=_NID
				end

				if not m1 then
					ui._curr.prsd=_NID
					ui._prev.prsd=_NID
				else
					if ui._curr.prsd==_NID then
						ui._curr.prsd=_NIT   -- this helps telling when mouse is down but no item is clicked
					else
						if ui._curr.prsd~=ui._kb_focus then
							ui._kb_focus=_NID
						end
					end
				end
				-- -- if no widget grabbed tab, clear focus
				if ui._key_entered==49 then -- TAB
					ui._kb_focus=_NID
				end
				ui._key_entered=_NOK
				ui._key_char=nil
				ui._text_changed=false
				if ui._curr.hovd==_NID then
					ui._prev.hovd=_NID
					if ui._curr.prsd==_NID and ui._kb_focus==_NID then
						ui._it_state_changed=false
					end
				end

				-- ---- HOUSEKEEPING -----------------------------------
				-- IDs cached at this point are those that haven't been claimed
				-- by any _items this frame in 'ui._new_item()'. That means
				-- they're orfan IDs from innactive ui items (hidden, gone, etc)
				-- and should be removed here.

				-- clean up orfan IDs
				for id,_ in pairs(ui._cache.i)do
					if ui._cache.h[id]then ui._cache.h[id]=nil end
					if ui._cache.p[id]then ui._cache.p[id]=nil end
				end
				-- gather what's left for next frame
				ui._cache.i=dmerge(ui._cache.h,ui._cache.p)

				------- HOUSEKEEPING DEBUG --------
				-- local hk_str = ""
				-- local lhi_str = ""
				-- local lpi_str = ""
				-- for k,_ in pairs(ui._cache.i) do hk_str = hk_str .. " | " .. tostring(k) end
				-- for k,_ in pairs(ui._cache.h) do lhi_str = lhi_str .. " | " .. tostring(k) end
				-- for k,_ in pairs(ui._cache.p) do lpi_str = lpi_str .. " | " .. tostring(k) end

				-- monitor("ui._cache.i    ", hk_str, 22)
				-- monitor("ui._cache.h  ", lhi_str, 22)
				-- monitor("ui._cache.p  ", lpi_str, 22)
			end
		--=--=--=--=--=--=--=--=--=--=--=--
		--=--=--=--=--=--=--=--=--=--=--=--
		--       Item Stuff
			function ui._make_id(it)
				if it.parent then return it.parent.id.."."..it.id end
				return it.id
			end

			function ui.begin_item(id,x,y,w,h,op)
				return ui._new_item(id,x,y,w,h,op)
			end

			function ui.end_item(it)
				if it.hovered then it:_set_as_last_hovered(true)end
				if it.pressed and it.hovered then it:_set_as_last_pressed(true)end
				ui._pop()
			end

			function ui.with_item(id,x,y,w,h,op,f)
				if ui.visible then
					local t=ui.begin_item(id,x,y,w,h,op)
						if ui.is_under_mouse(t.gx,t.gy,w,h) then
							ui.mouse_on_ui=true
						end
						f(t, t.args and unpk(t.args) or nil)-- -1)
					ui.end_item(t)
					return t
				end
			end

			function ui._set_none_hovered()ui._curr.hovd=_NID end
			function ui._set_none_pressed()ui._curr.prsd=_NID end
			function ui._is_none_pressed()return ui._curr.prsd==_NID end

			local _IDX_LUT = {
				-- global position
				gx=function(t)return t.parent and t.parent.gx+t.x or t.x end,
				gy=function(t)return t.parent and t.parent.gy+t.y or t.y end,
			}
			local Item={}
			local _ITMT={
				__index=function(t,k)
					if Item[k]~=nil then return Item[k]end
					return _IDX_LUT[k]and _IDX_LUT[k](t)
						or nil
				end,
				__tostring=function(t) -- only for debugging
					return fmt("%s(%s,%s,%s,%s,",t.id,t.gx,t.gy,t.w,t.h)
						.. fmt("\n  hovered: %s",t.hovered)
						-- .. fmt("\n  m_enter: %s", t.mouse_entered)
						-- .. fmt("\n  m_exit:  %s", t.mouse_exited)
						-- .. fmt("\n  held:    %s", t.held)
						-- .. fmt("\n  pressed: %s", t.pressed)
						-- .. fmt("\n  release: %s", t.released)
						.. "\n)"
				end,
			}
			function ui._new_item(id,x,y,w,h,op)
				op=op or {}
				local t=setmt({id=id,x=x,y=y,w=w,h=h},_ITMT)

				if #ui._items>0 then t.parent=ui._peek()end
				ui._push(t)
				t.id=ui._make_id(t)

				if ui.active then
					t.hovered=ui._is_cached_hovd(t.id)
					t.held=ui._is_cached_prsd(t.id)
				end
				ui._cache_item(t)

				if type(op)=="function"then op={code=op}end
				for k,v in pairs(op)do
					t[k]=v
				end
				return t
			end

			function ui.is_under_mouse(x,y,w,h)
				return mx>=x and mx<x+w
				   and my>=y and my<y+h
			end

			function ui.has_kb_focus(id)
				return ui._kb_focus==id
			end

			function Item._set_as_last_hovered(t,enable)
				if enable then
					ui._prev.hovd=t.id
					if not ui._is_cached_hovd(t.id)then
						ui._cache_hovd(t.id)
					end
				else
					ui._uncache_hovd(t.id)
				end
			end

			function Item._set_as_last_pressed(t,enable)
				if enable then
					ui._prev.prsd=t.id
					if not ui._is_cached_prsd(t.id) then
						ui._cache_prsd(t.id)
					end
				else
					ui._uncache_prsd(t.id)
				end
			end

			function Item._is_last_hovered(t)
				return ui._prev.hovd==t.id
					or ui._is_cached_hovd(t.id)
			end

			function Item._is_last_pressed(t)
				return ui._prev.prsd==t.id
					or ui._is_cached_prsd(t.id)
			end

			function Item.check_hovered(t,x,y,w,h)
				if not ui.locked and ui.active then
					if ui.is_under_mouse(x,y,w,h)then
						if not t.hovered and (ui._is_none_pressed() or t.held) then
							t.hovered=true
							ui._curr.hovd=t.id
						end
						if t.hovered then
							if not t:_is_last_hovered() then
								t.mouse_entered=true
								if t.tip then ui.info_item=t end
							end
							return true
						end
					else
						t.hovered=false
						if ui._curr.hovd==t.id then
							ui._set_none_hovered()
						end
						if t:_is_last_hovered()or t:_is_last_pressed()then
							t.mouse_exited=true
							t:_set_as_last_hovered(false)
						end
					end
				else
					t.hovered = false
					t:_set_as_last_hovered(false)
				end
				return false
			end

			function Item.check_pressed(t)
				if not ui.locked and ui.active then
					if m1 then
						if t.held then return true end
						if t.hovered and ui._is_none_pressed()then -- and not t.held then
							ui._curr.prsd=t.id
							if not t:_is_last_pressed() then
								t.pressed=true
							else
								t.held=true
							end
							return true
						end
					else
						if t.held then
							if ui._curr.prsd==t.id then
								ui._set_none_pressed()end
							t.held=false
							if t:_is_last_pressed()then
								t:_set_as_last_pressed(false)
								if t.hovered then
									t.released=true
								end
							end
						end
					end
				else
					t.pressed = false
					t.held = false
					t:_set_as_last_pressed(false)
				end
				return false
			end
		--=--=--=--=--=--=--=--=--=--=--=--

	--[[ TICkle Extensions                      006 ]]
		--[[ print with shadow rendering            001 ]]
			ui.add_render_step("prints",
				function(...)ui.push_render_step("prints",{...})end,
				prints
			)
		--[[ nframe rendering                       002 ]]
			local function _draw_quad(x,y,w,h,u,v,u2,v2)
				textri(x, y,   x+w,  y,    x+w, y+h,   u, v,   u2, v,    u2, v2)
				textri(x, y,   x+w, y+h,   x,   y+h,   u, v,   u2, v2,    u, v2)
			end
			ui.add_render_step("nframe",
				function(...)ui.push_render_step("nframe",{...})end,
				function(x,y,w,h,i,bw,s)
					i,bw,s=i or 0,bw or 3,s or 1

					local x2,x3,y2,y3=x+bw,x+w-bw,y+bw,y+h-bw
					local cw,ch=x3-x2,y3-y2  -- center w/h

					local UVS=8*s -- uv size

					local u,v=i%16*8,i//16*8
					local u4,v4=u+UVS,v+UVS
					local u2,v2,u3,v3=u+bw,v+bw,u4-bw,v4-bw

					--          x   y      w   h      u1  v1     u2  v2
					_draw_quad( x,  y,     bw, bw,    u,  v,     u2, v2 )  -- top left
					_draw_quad( x3, y,     bw, bw,    u3, v,     u4, v2 )  -- top right
					_draw_quad( x,  y3,    bw, bw,    u,  v3,    u2, v4 )  -- bottom left
					_draw_quad( x3, y3,    bw, bw,    u3, v3,    u4, v4 )  -- bottom right

					_draw_quad( x,  y2,    bw, ch,    u,  v2,    u2, v3 )  -- left
					_draw_quad( x3, y2,    bw, ch,    u3, v2,    u4, v3 )  -- right

					_draw_quad( x2, y,     cw, bw,    u2, v,     u3, v2 )  -- top
					_draw_quad( x2, y3,    cw, bw,    u2, v3,    u3, v4 )  -- bottom

					_draw_quad( x2, y2,    cw, ch,    u2, v2,    u3, v3 )  -- center
				end
			)
--=--=--=--=--=--=--=--=--=--=--=--=--
-- setup
	-- in-game persistent options / load values or defaults
	local webv,opts=false,{true,true,true,false,1}
	local USE_PADDING,WRAP_AROUND,RAND_START,RAND_RESET,ZOOM_LVL,CC_R,CC_G,CC_B=1,2,3,4,5,6,7,8

	-- original cell color = da7100 | 218,113,0
	local PALM=0x03FC0
	local cell_col=webv and {peek(PALM+8*3),peek(PALM+8*3+1),peek(PALM+8*3+2)} or nil
	function set_cell_color(c)
		cell_col=c
		local i,r,g,b=PALM+8*3,unpk(c)
		poke(i  ,r)
		poke(i+1,g)
		poke(i+2,b)

		pmem(CC_R,r)
		pmem(CC_G,g)
		pmem(CC_B,b)
	end


	if not webv then
		if pmem(USE_PADDING) ~= 1 then opts[USE_PADDING] = false end
		if pmem(WRAP_AROUND) ~= 1 then opts[WRAP_AROUND] = false end
		if pmem(RAND_START)  ~= 1 then opts[RAND_START]  = false end
		if pmem(RAND_RESET)  ~= 0 then opts[RAND_RESET]  = true end
		if pmem(ZOOM_LVL)==0 then pmem(ZOOM_LVL, opts[ZOOM_LVL]) end

		local mr,mg,mb=pmem(CC_R),pmem(CC_G),pmem(CC_B)
		-- tracec(mr,mg,mb)
		-- if mr~=r or mg~=g or mb~=b then
			set_cell_color({mr,mg,mb})
		-- end
		-- trace1d(cell_col)
	end

	function toggle_opt(i) -- toggle value of an option and save it
		local b=not opts[i]
		pmem(i,b and 1 or 0)
		opts[i]=b
		if i == USE_PADDING then
			set_padding(b)
		end
	end
	local upd_delay=0
	local g_mx,g_my,g_lmx,g_lmy=0,0,0,0 -- grid mouse pos
	local ctrl,shift,alt=false,false,false

	local pre,cur,sel=1,2,3 -- prev/curr/ghost buffer indices
	local cells={} -- cell buffers
	local CS,GW,GH=8//opts[ZOOM_LVL], 30*opts[ZOOM_LVL], 17*opts[ZOOM_LVL] -- cell size, grid width/height
	local zoom_mults={8,4,2,1}
	local pad,anim=0 -- padding for cell rects (always 0 if 'opts[ZOOM_LVL] < 8' -- see 'set_padding()')
	local paused,stopped,eraser,trippy=true,true,false,false
	local l_cells,gens,TOT_CELLS=0,0,0 -- living cells / generations
	local GAME_SCR,HELP_SCR1,HELP_SCR2,HELP_SCR3,HELP_SCR4,OPTS_SCR=1,2,3,4,5,10 -- game screens
	local NUM_HELP_SCRS,cur_scr=4,GAME_SCR

	-- http://www.mirekw.com/ca/ca_files_formats.html

	-- local STATICS,OSCILATORS,EXPLOSIVE,GLIDERS,GUNS=1,2,3,4,5
	local cats={
		"Statics",
		"Oscilators",
		"Amusing/Explosive",
		"Gliders",
		"Guns",
	}

	-- help strings
	--------------------------
	local help_strs={
		[[

		                1 Speed meter

		  1  2     3    2 Generation count

		                3 Alive|Dead/Total cells
		 4
		                4 Playback controls
		 5
		                5 Drawing tools

		                6 Zoom lvl/mouse pos
		   6   7    8
		                7 Tool tips

		                8 Options/Help
		]],
		[[
			H         Cycle help screens
			O         Toggle Options
			SPACE     Pause/play
			  +SHIFT  Reset (stop)
			ENTER     Randomize cells
			SHIFT-C/F Revive/kill all cells
			G         Next Generation (if paused)
			I         Toggle info
			TAB       Toggle UI
			PG-UP/DN  Zoom in/out (resets cells)
			P         Toggle padding (if zoom>2)
		]],
		[[
			LMB       Start Drawing/Commit Draw
			RMB       Erase(brush tool)/cancel
			MMB       Erase(non-brush tools)
		]],
		[[
			V/1       Brush tool
			2-6       Pattern tool/categories
			L         Line tool
			R/C       Rectangle/circle tools
			F         Fill tool
		]],
		[[
			E         Toggle eraser mode
			SHIFT     Proportional rect/circle
			CTRL      Filled rect/circle
			ALT       Centered rect/circle
			W         Expand brush/next pattern
			S         Shrink brush/prev pattern
		]],
		[[
			2         Statics
			3         Oscilators
			4         Amusing/Explosive
			5         Gliders
			6         Guns
		]]
	}
--=--=--=--=--=--=--=--=--=--=--=--=--

-- fwd decls
local tl,rand_cells

--=--=--=--=--=--=--=--=--=--=--=--=--
-- GUI
	local pb_rect={x=240//2-76//2,y=-2,            w=76,h=10}
	local tb_rect={x=-2,          y=136//2-80//2,  w=10,h=80}
	local thm={
		bg=0,
		fg=13,
		text=5,
		shad=3,
		outl=1,
		header=9,
		cell=8,
		select=11,
		erase=7,
		dim_text=14,
		btn={
			fg_n=3,bg_n=0,
			fg_h=4,bg_h=0,
			fg_p=5,bg_p=0,
		},
	}

	local info_vis=true
	function toggle_info()
		info_vis=not info_vis
	end

	function _btn_icon(t,icn)
		if not ui.active then return icn+3 end
		if t.hovered then
			if t.held then return icn+2 end
			return icn+1
		end
		return icn
	end

	-- function _btn_col(t)
	-- 	local b=thm.btn
	-- 	if t.hovered then
	-- 		if t.held then return b.fg_p,b.bg_p end
	-- 		return b.fg_h,b.bg_h
	-- 	end
	-- 	return b.fg_n,b.bg_n
	-- end

	function Label(id,x,y,tx,c,op) -- align, op)
		c=c or thm.text
		ui.with_item(id,x,y,0,0,op,function(t)
			if t.shadow then ui.prints(tx,t.gx,t.gy,c,thm.shad)
			else ui.print(tx,t.gx,t.gy,c)
			end
		end)
	end

	function Button(id,x,y,icn,op)
		return ui.with_item(id,x,y,8,8,op,function(t,...)
			t:check_hovered(t.gx,t.gy,t.w,t.h)
			t:check_pressed()
			if t.code then t:code(...)end
			ui.spr(_btn_icon(t,icn),t.gx,t.gy,0)
		end)
	end

	-- local function TButton(id,x,y,txt,op)
	-- 	local w=txtw(txt)+2
	-- 	return ui.with_item(id,x,y,w,8,op,function(t,...)
	-- 		t:check_hovered(t.gx,t.gy,w,t.h)
	-- 		t:check_pressed()
	-- 		if t.code then t:code(...)end
	-- 		if t.shadow then ui.prints(txt,t.gx,t.gy+2,_btn_col(t),t.shadow)
	-- 		else
	-- 			ui.print(txt,t.gx,t.gy+2,_btn_col(t),false,1,false)
	-- 		end
	-- 	end)
	-- end

	function GenInfo()
		-- ui.with_item("gi",x,y,w,h,nil,function(t)
		if ui.visible then
			local ty,tc,oc,cstr,tcstr,tstr=2,thm.text,thm.outl,tostr(l_cells),tostr(TOT_CELLS-l_cells),tostr(TOT_CELLS)
			printo("Gen: "..gens,2,ty,tc,oc,_,_,true)

			printo("C:"..rep(' ',6-#cstr)..cstr
			     .."|"..rep(' ',6-#tcstr)..tcstr
			     .."/"..rep(' ',6-#tstr)..tstr
				,2,ty+8,tc,oc,false,_,true)

			-- printo("Speed:"..(100-upd_delay),2,ty+16,tc,oc,_,_,true)
			printo("Speed:"..1-(1*(upd_delay/100)),2,ty+16,tc,oc,_,_,true)
		end
	end

	function Separator(x,y)
		ui.with_item("-",x,y,0,0,_,function(t)
			ui.spr(5,t.gx,t.gy,0)
		end)
	end

	function PlaybackBar(id,r,op)
		ui.with_item("pb",r.x,r.y,r.w,r.h,op,function(t)
			ui.nframe(r.x,r.y,r.w,r.h,3)
			local b1,b2,b3,b4,b5,b6,b7,b8,b9

			b1=Button("b_rand",2,1,16)
			b2=Button("b_zoom",10,1,19)
			Separator(15,1)
			b3=ui.with_active(upd_delay<100,Button,"b_back",22,1,32)
			b4=Button("b_stop",30,1,stopped and 67 or 64)

			if stopped then
				b5=Button("b_play",38,1,80)
				if b5.released then unpause()end
			elseif paused then
				b5=Button("b_play",38,1,83)
				if b5.released then unpause()end
			else
				b6=Button("b_pause",38,1,96)
				if b6.released then pause()end
			end

			b7=ui.with_active(upd_delay>0,Button,"b_fwd",46,1,48)

			Separator(52,1)
			b8=Button("b_opts",59,1,240)
			b9=Button("b_help",67,1,243)

			if b1.released then rand_cells()end
			if b2.released then inc_zoom()end
			if b4.released then pause(true)end

			if shift then
				if b3.held then dec_speed()end
				if b7.held then inc_speed()end
			else
				if b3.released then dec_speed()end
				if b7.released then inc_speed()end
			end

			if b8.released then toggle_options()end
			if b9.released then toggle_help()end
		end)
	end

	function Toolbar(id,r,op)
		ui.with_item("tb",r.x,r.y,r.w,r.h,op,function(t)
			ui.nframe(r.x,r.y,r.w,r.h,3)
			local ttp,b1,b2,b3,b4,b5,b6=tl.type
			b1=Button("b_brush",1,1, 112+(ttp~="brush"and 0 or(eraser and 4 or 2)))
			b2=Button("b_rect", 1,9, 128+(ttp~="rect"and 0 or(eraser and 4 or 2)))
			b3=Button("b_circ", 1,17,144+(ttp~="circle"and 0 or(eraser and 4 or 2)))
			b4=Button("b_line", 1,25,160+(ttp~="line"and 0 or(eraser and 4 or 2)))
			b5=Button("b_fill", 1,33,176+(ttp~="fill"and 0 or(eraser and 4 or 2)))
			b6=Button("b_patt", 1,41,192+(ttp~="pattern"and 0 or(eraser and 4 or 2)))
			if b1.released then tl:switch("brush")end
			if b2.released then tl:switch("rect")end
			if b3.released then tl:switch("circle")end
			if b4.released then tl:switch("line")end
			if b5.released then tl:switch("fill")end
			if b6.released then tl:switch("pattern")end

			ui.spr(4,t.gx+1,t.gy+49,0)

			if tl.type=="rect"then
				ui.spr(shift and 119 or 118,t.gx+1,t.gy+56,0)
				ui.spr(ctrl and 135 or 134,t.gx+1,t.gy+63,0)
				ui.spr(alt and 151 or 150,t.gx+1,t.gy+70,0)
			elseif tl.type=="circle"then
				ui.spr(shift and 167 or 166,t.gx+1,t.gy+56,0)
				ui.spr(ctrl and 183 or 182,t.gx+1,t.gy+63,0)
				ui.spr(alt and 199 or 198,t.gx+1,t.gy+70,0)
			end
		end)
	end

	function Spinbox(id,x,y,val,min,max,step,op)
		return ui.with_item(id,x,y,8,8,op,function(t,...)
			t.val=val
			local b1,b2=
				Button("b1",0,0,224),
				Button("b2",txtw(tostr(max))+10,0,227)
			if b1.pressed then val=wrap(val-step,min,max)end
			if b2.pressed then val=wrap(val+step,min,max)end
			if t.val~=val then
				if val<t.val then t.decreased=true
				elseif val>t.val then t.increased=true
				end
				t.val_changed=true
				t.val=val
			end
			if t.code then t:code(...)end
			Label("l1",9,1,tostr(t.val),14)
		end)
	end

	function Switch(id,x,y,is_on,op)
		local w,h=16,8
		return ui.with_item(id,x,y,w,h,op,function(t,...)
			t:check_hovered(t.gx,t.gy,w,h)
			t:check_pressed()

			if t.pressed then
				is_on=not is_on
				t.switched=true
			end
			t.is_on=is_on

			if t.code then t:code(...)end

			local handle=t.hovered and(is_on and 248 or 246)
				                    or(is_on and 232 or 230)

			ui.spr(handle,t.gx,t.gy,-1,1,0,0,2,1)
		end)
	end

	-- function ColorBar(id,x,y,val)
	-- 	local w=10*8,8
	-- 	ui.with_item(id,x,y,w,h,_,function(t, ...)
	-- 		Button("lb",0,0,214)
	-- 		Button("rb",72,0,230)
	-- 		for i=0,7 do
	-- 			-- 217
	-- 			-- 233
	-- 			ui.spr(217,t.gx+8*(i+1),t.gy,0)
	-- 		end
	-- 		local hx=t.gx+6+(val//8)
	-- 		trace(hx)
	-- 		ui.spr(218,hx,t.gy,0)
	-- 	end)
	-- 	return val
	-- end

	function CellColorPicker(id,x,y,c)
		local w,h,r,g,b=80,24,unpk(c)
		ui.with_item(id,x,y,w,h,_,function(t, ...)
			Label("l4",24,1,"Cell Color",thm.txt,{shadow=1})
			ui.spr(1,t.gx+88,t.gy)
			local s1,s2,s3

			-- cell_col[1]=ColorBar("br1",8,8,cell_col[1])
			s1=Spinbox("sb1",8, 8,r,0,255,1)
			s2=Spinbox("sb2",8,16,g,0,255,1)
			s3=Spinbox("sb3",8,24,b,0,255,1)
			if s1.val_changed then r=s1.val end
			if s2.val_changed then g=s2.val end
			if s3.val_changed then b=s3.val end
		end)
		return {r,g,b}
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Animation Player
	-- Really simple/naive animation player. Only animates
	-- positions. Only used for the UI.
	-- Probably overkill, but I didn't know any better.

	-- 		Animation
	local ANMT={}
	local function Animation(name,obj,dx,dy)
		return setmt({name=name,obj=obj,dx=dx,dy=dy,playing=false},ANMT)
	end
	ANMT={
		__index=function(t,k)return ANMT[k]end,
		play=function(t)t.playing=true end,
		update=function(t)
			local scalar,obj,dx,dy=0.70,t.obj,t.dx,t.dy
			obj:set_pos(lerp(obj.x,dx,scalar),lerp(obj.y,dy,scalar) )
			if abs(obj.x-dx)<0.1 then obj.x=dx end
			if abs(obj.y-dy)<0.1 then obj.y=dy end
			if obj.x==dx and obj.y==dy then
				obj:set_pos(dx,dy)
				t.playing=false
				return true
			end
			return false
		end,
	}

	-- 		Animation Player
	local ANMTRMT={}
	local function Animator()
		return setmt({anims={}},ANMTRMT)
	end
	ANMTRMT={
		__index=function(t,k)return ANMTRMT[k]end,
		update=function(t,dt)
			for _,a in pairs(t.anims)do
				if a.playing==true then a:update()end
			end
		end,
		play=function(t,name)
			t.anims[name]:play()
		end,
		is_playing=function(t,name)
			if not name then
				for _,a in pairs(t.anims)do
					if a.playing then return true end
				end
			else
				return t.anims[name].playing
			end
		end,
		add_anim=function(t,name,obj,dx,dy)
			t.anims[name]=Animation(name,obj,dx,dy)
		end,
	}
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Flood Fill
	-- TODO: revise this
	local function _has_pix(v,x,y,s,c,elipse)
		if elipse then
			return x<1 or y<1 or x>GW or y>GH
			or s[y][x]==1
		else
			return x<1 or y<1 or x>GW or y>GH
			or s[y][x]==1 or c[y][x]==v
		end
	end

	--   Scanline FT
	local function scnln_ft(x, y, ellipse,v)
		local pts={}
		ins(pts,vec2(x,y)) -- add the initial point

		local s,c,tidx,pt,set_abv,set_blw,sy=cells[sel],cells[cur],#pts
		repeat
			pt = rem(pts)
			set_abv,set_blw,sy,x=true,true,s[pt.y],pt.x
			while not _has_pix(v,x,pt.y,s,c,ellipse)do
				sy[x]=1
				if _has_pix(v,x,pt.y-1,s,c,ellipse)~=set_abv then
					set_abv=not set_abv
					if not set_abv then ins(pts,vec2(x,pt.y-1))end
				end
				if _has_pix(v,x,pt.y+1,s,c,ellipse)~=set_blw then
					set_blw=not set_blw
					if not set_blw then ins(pts,vec2(x,pt.y+1))end
				end
				x=x+1
			end
			set_abv=pt.y>0 and _has_pix(v,pt.x,pt.y-1,s,c,ellipse)
			set_blw=pt.y<GH-1 and _has_pix(v,pt.x,pt.y+1,s,c,ellipse)
			x=pt.x-1
			while not _has_pix(v,x,pt.y,s,c,ellipse)do
				sy[x]=1
				if _has_pix(v,x,pt.y-1,s,c,ellipse)~=set_abv then
					set_abv=not set_abv
					if not set_abv then ins(pts,vec2(x,pt.y-1))end
				end
				if _has_pix(v,x,pt.y+1,s,c,ellipse)~=set_blw then
					set_blw=not set_blw
					if not set_blw then ins(pts,vec2(x,pt.y+1))end
				end
				x=x-1
			end
		until #pts==0
	end

	local function flood_fill(x,y,ellipse)
		elipse=elipse or false
		scnln_ft(x,y,ellipse,eraser and 0 or 1)
	end
--=--=--=--=--=--=--=--=--=--=--=--=--
	--[[ sdist - distance squared              001 ]]
		local function sdist(x1,y1,x2,y2)local a,b=x1-x2,y1-y2 return a*a+b*b end
	--[[ Bounds checking                       003 ]]
		local function inbounds(x,y)
			return x>0 and x<=GW and y>0 and y<=GH
		end
--=--=--=--=--=--=--=--=--=--=--=--=--
-- Geometry stuff
	local function point(x,y)
		return {x=x,y=y}
	end

	local geom={}
	geom={
		--TODO: these functiosn shouldn't draw anything,
		--      they should just return a list of points
		_rect_hollow=function(r)
			local t={}
			for j=r.y,r.y2 do
				t[#t+1]=point(r.x,j)
				t[#t+1]=point(r.x2,j)
			end
			for i=r.x,r.x2 do
				t[#t+1]=point(i,r.y)
				t[#t+1]=point(i,r.y2)
			end
			return t
		end,
		_rect_filled=function(r)
			local t={}
			for j=r.y,r.y2 do
				for i=r.x,r.x2 do
					t[#t+1]=point(i,j)
				end
			end
			return t
		end,
		_circle_filled=function(x,y,x1,y1,x2,y2,r)
			local t,R={},r*r
			for j=y-r,y+r do
				for i=x-r,x+r do
					if sdist(i,j,x,y)<=R+1 then
						t[#t+1]=point(i,j)
					end
				end
			end
			return t
		end
	}
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Tools
	tl = {
		type="brush",
		orgn=nil,
		w=0,h=0,
		mode1=false,  -- filled
		mode2=false,  -- square
		mode3=false,  -- centered on mouse
		drawing=false,
		brush_size=0,
		max_size=10,
		cats={},
		cur_pats={},
		cur_cat=1,
		cur_pat=1,
	}
	function tl.start(t,x, y)
		t.orgn = vec2(x, y)
		t.drawing = true
	end

	function tl.stop(t)
		t.drawing = false
	end

	function tl.toggle(t,x, y)
		if not t.drawing then
			t:start(x, y)
		else
			t:stop()
		end
	end

	function tl.cancel(t)
		if t.type ~= nil then
			t:stop()
		end
	end

	function tl.commit(t,draw)
		local s,c,sj,cj=cells[sel],cells[cur]
		for j=1,GH do
			sj,cj=s[j],c[j]
			for i=1,GW do
				if sj[i]==1then
					cj[i]=draw and 1 or 0
				end
			end
		end
	end

	function tl.clear(t)
		local s,sj=cells[sel]
		for j=1,GH do
			sj=s[j]
			for i=1,GW do
				sj[i]=0
			end
		end
	end

	function tl.switch(t,tp, force)
		if t.type~=tp or force then
			t:cancel()
			t.type = tp
		end
	end

	function tl.expand(t)
		t.brush_size = min(t.brush_size+1, t.max_size)
	end

	function tl.contract(t)
		t.brush_size = max(t.brush_size-1, 0)
	end

	-- tl.brush_type="square"
	tl.brush_type="round"
	function tl._brush_pts(t,x,y)
		-- TODO: improve this crappy brush
		-- r=radius
		local path= t.drawing and Bres.line(g_lmx,g_lmy,g_mx,g_my) or {point(x,y)}
		-- local path=Bres.line(g_lmx,g_lmy,x,y)
		local set,p={}
		local pts,R={},t.brush_size
		for i=1, #path do
			local p=path[i]
			local xmin,ymin,xmax,ymax=
					max(1, p.x-R),
					max(1, p.y-R),
					min(GW, p.x+R),
					min(GH, p.y+R)

			if t.brush_type=="square" then    pts=geom._rect_filled(rec4(xmin,ymin,xmax-xmin,ymax-ymin))
			elseif t.brush_type=="round" then pts=geom._circle_filled(p.x,p.y,xmin,ymin,xmax,ymax,R)
			end

			for _,v in ipairs(pts)do
				set[v]=1
			end
		end

		pts={}
		for k,v in pairs(set) do
			pts[#pts+1]=k
		end
		tl._commit_pts(pts)
	end

	function tl._commit_pts(pts)
		local s=cells[sel]
		for _,p in ipairs(pts)do
			if inbounds(p.x,p.y) then
				s[p.y][p.x]=1
			end
		end
	end

	function tl._base_rect(t,x,y)
		local o,p1,p2,s,r=t.orgn,{}
		p1,p2=vec2(min(x,o.x),min(y,o.y)),vec2(max(x,o.x),max(y,o.y))
		s=p2-p1
		if t.mode2 then -- if square
			s=vec2(min(s.x,s.y),min(s.x,s.y))
			if x<o.x then p1.x=p1.x+(o.x-(p1.x+s.x))end
			if y<o.y then p1.y=p1.y+(o.y-(p1.y+s.y))end
		end
		return rec2(p1,s)
	end

	function tl._rect_pts(t,x,y)
		if t.drawing then
			local r,pts=tl:_base_rect(x,y)
			if t.mode3 then r.p=t.orgn-r.s//2 end -- if centered
			if t.mode1 then pts=geom._rect_filled(r)
			else            pts=geom._rect_hollow(r)
			end
			tl._commit_pts(pts)
		end
	end

	function tl._circle_pts(t,x,y)
		if t.drawing then
			local r,pts,hw,hh=tl:_base_rect(x,y)
			if t.mode3 then r.p=t.orgn-r.s//2 end -- if centered
			hw,hh=r.w//2,r.h//2
			pts=Bres.ellipse(r.x+hw,r.y+hh,hw,hh)
			tl._commit_pts(pts)
			if t.mode1 then flood_fill(r.c.x,r.c.y,true)end
		end
	end

	function tl._line_pts(t,x, y)
		if t.drawing then
			tl._commit_pts(Bres.line(t.orgn.x,t.orgn.y,x,y))
		end
	end

	function tl._fill_pts(t,x, y)
		flood_fill(x, y)
	end

	function tl._patt_pts(t,x, y)
		local s,p,gy,sj,pj,gx=cells[sel],t.cats[t.cur_cat][t.cur_pat].layout
		for j=1,t.h do
			gy=j+(y-1)-t.h
			if gy>0 and gy<=GH then
				sj,pj=s[gy],p[j]
				for i=1,t.w do
					gx=i+(x-1)-t.w
					if gx>0 and gx<=GW then
						sj[gx]=pj[i]
					end
				end
			end
		end
	end

	tl._draw_fns = {
		brush   = tl._brush_pts,
		rect    = tl._rect_pts,
		circle  = tl._circle_pts,
		line    = tl._line_pts,
		fill    = tl._fill_pts,
		pattern = tl._patt_pts,
	}

	function tl.draw_points(t,x, y)
		t._draw_fns[t.type](t,x, y)
	end

	function tl._set_size(t)
		t.w = t.cats[t.cur_cat][t.cur_pat].w
		t.h = t.cats[t.cur_cat][t.cur_pat].h
	end

	function tl.set_category(t,c)
		t.cur_cat = c
		t.cur_pat = t.cur_pats[t.cur_cat]
		t:_set_size()
	end

	function tl.next_pattern(t)
		local cat = t.cur_cat
		local old_pat, new_pat = t.cur_pat, clamp(t.cur_pats[cat]+1, 1, #t.cats[cat])
		if new_pat ~= old_pat then
			t.cur_pat = new_pat
			t.cur_pats[cat] = new_pat
			t:_set_size()
		end
	end

	function tl.prev_pattern(t)
		local old_pat, new_pat = t.cur_pat, clamp(t.cur_pats[t.cur_cat]-1, 1, #t.cats[t.cur_cat])
		if new_pat ~= old_pat then
			t.cur_pat = new_pat
			t.cur_pats[t.cur_cat] = new_pat
			t:_set_size()
		end
	end

	function tl._parse_patt(t,p)
		local _add_cell,_getnum=
			function(a,v,n)
				for _=1,n do a[#a+1]=v end
			end,
			function(i,str)
				local s,nex=str:sub(i,i)
				while i<i+4 do -- inf loop guard
					i=i+1
					nex=str:sub(i,i)
					if nex=="."or nex=="o" then break end
					s=s..nex
				end
				return tonum(s),i
			end

		local lns=p:split(' ')
		local w,h,name,pat,i,c,num,row=tonum(lns[1]),tonum(lns[2]),lns[3],{}
		for i=1,3 do rem(lns,1)end
		for _,l in ipairs(lns) do
			i,row=1,{}
			while i <= w do
				c=l:sub(i,i)
				if c==""then
					_add_cell(row,0,w-#row)
					break
				end
				num=tonum(c)
				if not num then
					_add_cell(row,c=="o" and 1 or 0,1)
					i=i+1
				else
					num,i=_getnum(i,l)
					c=l:sub(i,i)
					_add_cell(row,c=="o" and 1 or 0,num)
					i=i+1
				end
			end
			pat[#pat+1]=row
		end
		return {w=w,h=h,name=name,layout=pat}
	end

	function tl.init_pats(t)
		local pats={
			{ -- statics
				"3 4 '' .o o.o o.o .o",
				"4 4 'Loaf' .2o o2.o .o.o 2.o",
			},
			{ -- oscilators
				"4 2 'Toad' .3o 3o",
				"3 10 '' .o .o o.o .o .o .o .o o.o .o .o",
				"4 4 '' 2o o 3.o 2.2o",
				"13 13 'Pulsar' 2.3o3.3o . o4.o.o4.o o4.o.o4.o o4.o.o4.o 2.3o3.3o . 2.3o3.3o o4.o.o4.o o4.o.o4.o o4.o.o4.o . 2.3o3.3o",
				"15 18 '' 8.2o2.2o 8.o4.o 9.4o 6.3o3.o.o 2.o3.o2.o3.2o .o.o3.o.o o.o2.o.o.2o o2.2o.o3.o .o.o2.o.2o 2o.o.o.o2.o o2.o.o.o.2o .2o6.o 3.5o.o 3.o4.o 4.4o . 4.2o 4.2o",
			},
			{ -- explosive
				"3 3 'R_Pentomino' .2o 2o .o",
				"8 2 '' 2o4.o .o3.3o",
				"29 10 '' 2o14.2o9.2o 2o15.2o8.2o 13.5o 13.4o12. . 3.2o8.4o 2.o.o.2o5.5o .2o3.2o9.2o8.2o 2.o.o.2o8.2o9.2o 3.2o",
				"39 1 '' 8o.5o3.3o6.7o.5o",
				"7 3 '' .o 3.o 2o2.3o",
				"5 5 '' 3o.o o 3.2o .2o.o o.o.o",
				"8 6 '' 6.o 4.o.2o 4.o.o 4.o 2.o o.o",
			},
			{ -- gliders
				"3 3 'Glider' 2.o o.o .2o",
				"5 4 'Light-Weight_Spaceship' o2.o 4.o o3.o .4o",
			},
			{ -- guns
				"36 9 'Glider_Gun' 24.o11. 22.o.o 12.2o6.2o12.2o 11.o3.o4.2o12.2o 2o8.o5.o3.2o 2o8.o3.o.2o4.o.o 10.o5.o7.o 11.o3.o 12.2o",
			},
		}
		for j,c in ipairs(pats) do
			t.cur_pats[j]=1
			t.cats[j]={}
			for i,p in ipairs(c) do
				t.cats[j][i]=t:_parse_patt(p)
			end
		end
		t:_set_size()
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- init
	function toggle_padding()
		toggle_opt(USE_PADDING)
		set_padding(opts[USE_PADDING])
	end

	function set_padding(bool)
		pad = bool and (CS < 4 and 0 or 1) or 0
	end

	function rand_cells(rst)
		l_cells=0
		if rst then pause(true)end
		local cc,b=cells[cur],0
		for j=1,GH do
			ccj=cc[j]
			for i=1,GW do
				b=rand()<0.5 and 1 or 0
				ccj[i]=b
				l_cells=l_cells+b
			end
		end
	end

	function create_cells()
		local b1,b2,b3={},{},{}
		for j=0,GH+1 do
			b1[j],b2[j],b3[j]={},{},{}
			for i=0,GW+1 do
				b1[j][i],b2[j][i],b3[j][i]=0,0,0
			end
		end
		cells={b1,b2,b3}
		TOT_CELLS=GW*GH
	end

	function fill_grid(fill)
		local cc,v,ccj=cells[cur],fill and 1 or 0
		for j=1,GH do
			ccj=cc[j]
			for i=1,GW do
				ccj[i]=v
			end
		end
		l_cells=(fill and GW*GH or 0)
	end



	function dec_zoom()set_zoom(opts[ZOOM_LVL]-1,true)end
	function inc_zoom()set_zoom(opts[ZOOM_LVL]+1,true)end

	function set_zoom(val, wrp, force)
		local limit = wrp and wrap or clamp
		val = limit(val, 1, 4)

		local n=zoom_mults[val]

		if val ~= opts[ZOOM_LVL] or force then
			CS = 8//n
			GW = 30*n
			GH = 17*n
			opts[ZOOM_LVL] = val
			set_padding(opts[USE_PADDING])
			gens = 0
			l_cells = 0
			create_cells()
			TOT_CELLS = GW*GH
			if not paused then pause() end
			pmem(ZOOM_LVL, val)
		end
	end

	function init()
		set_padding(opts[USE_PADDING])
		tl.orgn = vec0()
		tl:init_pats()
		-- create_cells()
		set_zoom(pmem(ZOOM_LVL),false,true)
		if opts[RAND_START] then rand_cells() end

		anim = Animator()

		-- init_ui()

		-- ui.tlbar.tls["brush"]:toggle_state()
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- update

	local function compute_gen()
		cur,pre=pre,cur
		-- the weirdness in here got me over 10ms and 20fps
		local lc,p,c=0,cells[pre],cells[cur]	-- count alive cells, and make buffers local
		local l,r,n,b,cj,pj,pu,pd -- this seems to give me ~2ms (and more token budget)

		for j=1,GH do
			-- make buffer rows local here for faster access
			cj,pj,pu,pd=c[j],p[j],p[j-1],p[j+1]
			for i=1,GW do
				l,r=i-1,i+1
				-- count alive neighbors
				n=pu[l]+pu[i]+pu[r]+pj[l]+pj[r]+pd[l]+pd[i]+pd[r]
				-- apply rules
				b=(n==3or(n==2 and pj[i]==1))and 1 or 0
				cj[i]=b
				lc=lc+b
			end
		end

		if opts[WRAP_AROUND] then  -- swap borders
			for i=0,GW-1 do
				c[GH+1][i],c[0][i]=c[1][i],c[GH][i]
			end
			for j=1,GH do
				c[j][GW+1],c[j][0]=c[j][1],c[j][GW]
			end
		end
		l_cells=lc
		gens=gens+1
	end

	local function update_ui()
	bma("ui update",function()--@bm
		if cur_scr == GAME_SCR then
			PlaybackBar("pb", pb_rect)
			Toolbar("tb",tb_rect)
		end
	end)--@bm
	end

	local function update()
	bma("update",function()--@bm
		if cur_scr == GAME_SCR then
			-- if use_ibar then ui.lbl_nfo:set_text("") end

			if not ui.mouse_on_ui then
				tl:clear()
				tl:draw_points(g_mx,g_my)
			end
			-- ui.lbl_mouse:set_text(opts[ZOOM_LVL].." | "..g_mx..", "..g_my)

			bma("compt gen", function()
				if not paused and (upd_delay==0 or f%upd_delay==0) then
					compute_gen()
				end
			end)--@bm

			anim:update()
			-- update_ui()
		elseif cur_scr == OPTS_SCR then
			-- update_ui()
		end
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- render
	local function render_rects()
		local rect,s,c,rs,ca,cs,sj,cj,x,y=rect,cells[sel],cells[cur],CS-pad,thm.cell,(eraser and thm.erase or thm.select)
		for j=1,GH do
			y=(j-1)*CS+pad
			sj,cj=s[j],c[j]
			for i=1,GW do
				x=(i-1)*CS+pad
				if sj[i]==1 and not ui.mouse_on_ui then rect(x,y,rs,rs,cs)
				elseif cj[i]==1 then rect(x,y,rs,rs,ca)
				end
			end
		end
		if not tl.drawing and not ui.mouse_on_ui then
			rect((g_mx-1)*CS+pad,(g_my-1)*CS+pad,CS-pad,CS-pad,cs)
		end
	end

	local function render_pix()
		local pix,s,c,ca,cs,sj,cj,y=pix,cells[sel],cells[cur],thm.cell,(eraser and thm.erase or thm.select)
		for j=1,GH do
			y,sj,cj=j-1,s[j],c[j]
			for i=1,GW do
				if sj[i]==1 and not ui.mouse_on_ui then pix(i-1,y,cs)
				elseif cj[i]==1 then pix(i-1,y,ca)
				end
			end
		end
		if not tl.drawing and not ui.mouse_on_ui and tl.type~="pattern" then
			pix(g_mx-1,g_my-1,cs)
		end
	end

	local function render_rects_trippy()
		local rect,s,c,rs,ca,cs,sj,cj,x,y=rect,cells[sel],cells[cur],CS-pad,thm.cell,(eraser and thm.erase or thm.select)
		for j=1,GH do
			y=(j-1)*CS+pad
			sj,cj=s[j],c[j]
			for i=1,GW do
				x=(i-1)*CS+pad
				if sj[i]==1 and not ui.mouse_on_ui then rect(x,y,rs,rs,cs)
				elseif cj[i]==1 then rect(x,y,rs,rs,rand(0,15))
				end
			end
		end
		if not tl.drawing and not ui.mouse_on_ui then
			rect((g_mx-1)*CS+pad,(g_my-1)*CS+pad,CS-pad,CS-pad,cs)
		end
	end

	local function render_pix_trippy()
		local pix,s,c,ca,cs,sj,cj,y=pix,cells[sel],cells[cur],thm.cell,(eraser and thm.erase or thm.select)
		for j=1,GH do
			y,sj,cj=j-1,s[j],c[j]
			for i=1,GW do
				if sj[i]==1 and not ui.mouse_on_ui then pix(i-1,y,cs)
				elseif cj[i]==1 then pix(i-1,y,rand(0,15))
				end
			end
		end
		if not tl.drawing and not ui.mouse_on_ui and tl.type~="pattern" then
			pix(g_mx-1,g_my-1,cs)
		end
	end

	local function render_ui()
	bma("ui_render",function()--@bm
		if cur_scr == GAME_SCR and info_vis then
			GenInfo()
		end
	end)--@bm
	end

	local function draw_game()
		if CS > 1 then render_rects()
		else           render_pix()
		end
		render_ui()
	end

	local function draw_help()
		cls(0)
		local tc,hc,sc=thm.text,thm.header,thm.shad
		printgsc("Help "..cur_scr-1 .."/"..NUM_HELP_SCRS,_,0,hc)
		printgsc("H >>",_,16,hc,false)
		if cur_scr==HELP_SCR1 then
			printgsc("Screen",1,1,hc)
			printgs(help_strs[1]:gsub('\t',''),0,2,tc,sc,true)
			spr(256,2*8,5*8+5,0,1,0,0,8,5)
		elseif cur_scr==HELP_SCR2 then
			printgsc("Controls",1,1,hc)
			printgs(help_strs[2]:gsub('\t',''),2,2,tc,sc,true)
			printgsc("Mouse editing",1,12,hc)
			printgs(help_strs[3]:gsub('\t',''),2,13,tc,sc,true)
		elseif cur_scr==HELP_SCR3 then
			printgsc("Tools",1,1,hc)
			printgs(help_strs[4]:gsub('\t',''),2,2,tc,sc,true)
			printgsc("Tool modes",1,7,hc)
			printgs(help_strs[5]:gsub('\t',''),2,8,tc,sc,true)
		elseif cur_scr==HELP_SCR4 then
			printgsc("Pattern Categories",1,2,hc)
			printgs(help_strs[6]:gsub('\t',''),2,3,tc,sc,true)
		end
	end

	local function draw_options()
		cls(1)
		local tc,hc,lbt,oc,c=thm.text,thm.header,{shadow=1},cell_col
		printgsc("Options ",_,1,hc)
		printgsc("O >>",_,16,hc,false)

		for i=1,#opts-1 do
			Switch("s"..i,16,32+8*(i-1),opts[i],function(t)
				if t.switched then toggle_opt(i)end
			end)
		end

		Label("l1",34,32+1,"use cell padding",tc,lbt)
		Label("l2",34,40+1,"wrap around edges",tc,lbt)
		Label("l3",34,48+1,"randomize at startup",tc,lbt)
		Label("l4",34,56+1,"reset on randomize",tc,lbt)

		Spinbox("sp1",16,72,opts[ZOOM_LVL],1,4,1,function(t)
			if t.val_changed then set_zoom(t.val,true)end
		end)
		Label("l4",40,72+1,"zoom level",tc,lbt)

		local c=CellColorPicker("cp",48,100,oc)
		if c[1]~=oc[1] or c[2]~=oc[2] or c[3]~=oc[3] then
			set_cell_color(c)
		end

		render_ui()
	end

	local function render()
	bma("render",function()--@bm
		cls(thm.bg)
		if     cur_scr < HELP_SCR1 then draw_game()
		elseif cur_scr < OPTS_SCR then draw_help()
		else draw_options()
		end
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--

function inc_color()
	local i=PALM+8*3
	local r,g,b=
		wrap(peek(i  )-1,0,255),
		wrap(peek(i+1)-1,0,255),
		wrap(peek(i+2)-1,0,255)

	set_cell_color({r,g,b})
	-- poke(i,r)
	-- poke(i+1,g)
	-- poke(i+2,b)
end

function dec_color()
	local i=PALM+8*3
		local r,g,b=
		wrap(peek(i  )+1,0,255),
		wrap(peek(i+1)+1,0,255),
		wrap(peek(i+2)+1,0,255)

	set_cell_color({r,g,b})
	-- poke(i,r)
	-- poke(i+1,g)
	-- poke(i+2,b)
end


--=--=--=--=--=--=--=--=--=--=--=--=--
-- input
	local function handle_keys()
		local k = keys
		if keyp(dbg.key) then dbg:toggle()
		else
			if     cur_scr >= OPTS_SCR then
				if keyp(k.O) then toggle_options() end
			elseif cur_scr >= HELP_SCR1 then
				if keyp() or mbtnp() then toggle_help() end
			else
				ctrl = key(k.CTRL)
				shift = key(k.SHFT)
				alt = key(k.ALT)

				tl.mode1 = ctrl
				tl.mode2 = shift
				tl.mode3 = alt

				if     keyp(k.SPACE)                then if shift then pause(true) else toggle_pause() end
				elseif keyp(k.G,10,5)               then if paused then compute_gen() end
				elseif keyp(k.ENTER)                then rand_cells(opts[RAND_RESET])
				elseif keyp(k.PGUP)                 then dec_zoom()
				elseif keyp(k.PGDN)                 then inc_zoom()

				elseif shift and key(k.UP)          then inc_speed()
				elseif shift and key(k.DOWN)        then dec_speed()
				elseif keyp(k.UP)                   then inc_speed()
				elseif keyp(k.DOWN)                 then dec_speed()
				elseif key(k.LEFT)                  then inc_color()
				elseif key(k.RIGHT)                 then dec_color()

				elseif keyp(k.O)                    then toggle_options()
				elseif keyp(k.H)                    then toggle_help()
				-- elseif keyp(k.P)                    then ui.cbx_pad:toggle_state()

				elseif keyp(k.TAB)                  then toggle_ui()
				elseif keyp(k.I)                    then toggle_info()
				elseif keyp(k.E)                    then toggle_eraser()
				elseif keyp(k.D)or keyp(k.B)or keyp(k.N1)then tl:switch("brush")
				-- elseif keyp(k.U)                    then ui.tlbar_handle:reset()
				elseif keyp(k.L)                    then tl:switch("line")
				elseif keyp(k.R)                    then tl:switch("rect")
				elseif keyp(k.C) and     shift      then fill_grid(false)
				elseif keyp(k.C) and not shift      then tl:switch("circle")
				elseif keyp(k.F) and     shift      then fill_grid(true)
				elseif keyp(k.F) and not shift      then tl:switch("fill")
				elseif keyp(k.W, 10, 5) then
					if     tl.type == "brush"   then tl:expand()
					elseif tl.type == "pattern" then tl:next_pattern()
					end
				elseif keyp(k.S, 10, 5) then
					if     tl.type == "brush"   then tl:contract()
					elseif tl.type == "pattern" then tl:prev_pattern()
					end
				elseif keyp(k.N2) then
					tl:switch("pattern")
					tl:set_category(1)
				elseif keyp(k.N3) then
					tl:switch("pattern")
					tl:set_category(2)
				elseif keyp(k.N4) then
					tl:switch("pattern")
					tl:set_category(3)
				elseif keyp(k.N5) then
					tl:switch("pattern")
					tl:set_category(4)
				elseif keyp(k.N6) then
					tl:switch("pattern")
					tl:set_category(5)
				end
			end
		end
	end

	local function handle_mouse()
		g_mx,g_my=mx//CS+1,my//CS+1
		g_lmx,g_lmy=lmx//CS+1,lmy//CS+1

		if cur_scr == GAME_SCR and not ui.mouse_on_ui then
			if mbtnp(M2) then
				tl:cancel()
			else
				if tl.type=="brush" then
					if     mbtn(M1) then
						tl:start()
						tl:commit(not eraser)
					elseif mbtn(M2) then
						tl:start()
						tl:commit(false)
					elseif mbtnr(M1) then tl:stop()
					elseif mbtnr(M2) then tl:stop()
					end
				elseif mbtnp(M1) then
					if tl.type == "line" then
						if tl.drawing then tl:commit(not eraser) end
						tl:start(g_mx, g_my)
					elseif (tl.type == "fill" or tl.type == "pattern") or tl.drawing then
						tl:commit(not eraser)
						tl:cancel()
					else
						tl:toggle(g_mx, g_my)
					end
				elseif mbtnp(M3) then
					tl:commit(false)
					tl:cancel()
				end
			end
		end
	end

	local function input()
	bma("input",function()--@bm
		if not ui.mouse_on_ui then
			handle_mouse()
		end
		handle_keys()
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Unsorted stuff

	function inc_speed()
		upd_delay=clamp(upd_delay-1,0,100)
	end

	function dec_speed()
		upd_delay=clamp(upd_delay+1,0,100)
	end

	function toggle_ui()
		ui.visible=not ui.visible
	end

	function toggle_help()
		if cur_scr == GAME_SCR then
			cur_scr = HELP_SCR1
		else
			cur_scr = wrap(cur_scr+1, 1, NUM_HELP_SCRS+1)--%(NUM_HELP_SCRS+1)
		end
	end

	function toggle_options()
		if cur_scr == GAME_SCR then
			cur_scr = OPTS_SCR
		else
			cur_scr = GAME_SCR
		end
	end

	function unpause()
		if paused then
			stopped=false
			paused=false
		end
	end

	function pause(restart)
		if not paused or restart then
			paused = true
			if restart then reset()end
		end
	end

	function toggle_pause()
		if paused then unpause()
		else pause()
		end
	end

	function toggle_eraser()
		eraser = not eraser
	end

	function reset()
		gens = 0
		fill_grid(false)
		stopped=true
	end

	function set_speed(dir)

	end
--=--=--=--=--=--=--=--=--=--=--=--=--


function TIC()
bma("whole",function()--@bm

	tm_check()
	update_mst()

	monitor("dt", fmt("%.3f",dt), 11)
	monitor("fps", fmt("%d",1//dt), 11)

	ui.start_frame()
	update_ui()
	input()
	update()
	render()
	monitor("mouse_ui", ui.mouse_on_ui,11)
	ui.end_frame()

	-- local i=PALM+8*3
	-- local r,g,b=peek(i),peek(i+1),peek(i+2)
	-- monitor("col", "("..r..","..g..","..b..")",8)

		dbg:draw()
end)--@bm
end


init()
