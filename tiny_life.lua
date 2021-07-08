
-- title: Tiny Life
-- author: Skaruts (MetalDudeBro)
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
	--[[ tracef - trace formatted              001 ]] function tracef(...) trace(fmt(...)) end
	--[[ tracec - trace csv arguments          003 ]] function tracec(...) local t={} for i=1,select("#",...)do t[i]=tostr(select(i,...)) end trace(conc(t,",")) end
	--[[ trace1d - trace a 1D array            001 ]] local function trace1d(t) local s=""for i,v in ipairs(t)do s=s..v..","end trace(s) end
	--[[ trace2d - trace a 2d array            003 ]] function trace2d(a,sep,i0) sep=sep or""local s,c=i0 and i0 or 1 for j=s,#a do c={} for i=s,#a[i0] do c[i+1-s]=tostr(a[j][i]) end trace(conc(c,sep)) end end

	--[[ array2 - make new 2d array with 'v'   002 ]] function array2(w,h,v) v=v or 0 local t={} for j=1,h do t[j]={} for i=1,w do t[j][i]=v end end return t end

	--[[ rotate/flip 2d arrays (WIP)           000 ]]
		function rot90(inp)
			local w,h=#inp[1],#inp
			local out=array2(h,w)
			for j=1,h do
				for i=1,w do
					out[i][h+1-j]=inp[j][i]
				end
			end
			return out
		end
		function rotate(a,n)
			n=n%4
			if n>0 then
				for i=1,n do a=rot90(a)end
			end
			return a
		end
		function hflip(inp)
			local w,h=#inp[1],#inp
			local out=array2(w,h)
			for j=1,h do
				for i=1,w do
					out[j][w+1-i]=inp[j][i]
				end
			end
			return out
		end
		function vflip(inp)
			local w,h=#inp[1],#inp
			local out=array2(w,h)
			for j=1,h do
				for i=1,w do
					out[h+1-j][i]=inp[j][i]
				end
			end
			return out
		end
		-- local foo={
		-- 	{1,2,3,4},
		-- 	{5,6,7,8},
		-- }
		-- trace2d(foo,'',1)
		-- foo = rotate(foo,-1) trace2d(foo,'',1)  -- 90
		-- foo = hflip(foo,1) trace2d(foo,'',1)  -- 90
		-- foo = vflip(foo,1) trace2d(foo,'',1)  -- 90

	--[[ sdist - distance squared              001 ]] local function sdist(x1,y1,x2,y2)local a,b=x1-x2,y1-y2 return a*a+b*b end
	--[[ tm_check - time stuff / dt            003 ]] local f,t1,t2,dt,tsecs,tm_check=0,time() function tsecs(ms) return (ms or time())/1000 end function tm_check() f=f+1 t2=time() dt=(t2-t1)/1000 t1=t2 end
	--[[ has - check if 'o' is in table 't'    001 ]] local function has(t,o) for i=1,#t do if t[i]==o then return true end end end
	--[[ dmerge - merge dict/hash tables       001 ]] local function dmerge(a,b,err,...) local has,type=has,type err=err or"Key '%s' already exists in table."local t={} for k,v in pairs(a)do if type(k)~="number"then t[k]=v end end for k,v in pairs(b)do if type(k)~="number" then if has(t,k)then print(fmt(err,...or k)) else t[k]=v end end end return t end

	--[[ clamp - keep v between l and h        002 ]] local function clamp(v,l,h)return max(l,min(v,h))end
	--[[ wrap - wrap v around l and h          003 ]] local function wrap(v,l,h) return v > h and l or v<l and h or v end
	--[[ round - round v to nearest int        003 ]] local function round(v)return floor(v+0.5)end
	--[[ lerp - linear interpolate             002 ]] local function lerp(a,b,t)return a*(1-t)+b*t end

	--[[ txtw - get text width                 002 ]] local function txtw(tx,fw,s,sf) return print(tx,0,-99,-1,fw or false,s or 1,sf or false) end
	--[[ prints - print with shadow            003 ]] local function prints(tx,x,y,c,sc,fw,s,sf) fw,s,sc,sf=fw or false,s or 1,sc or 1,sf or false print(tx,x+1,y+1,sc,fw,s,sf) print(tx,x,y,c,fw,s,sf) end
	--[[ printgs - print on grid with shadow   009 ]] local function printgs(t,x,y,c,sc,fw,s,ox,oy) fw,s,sc=fw or false,s or 1,sc or 1 x,y=x*8+1+(ox or 0),y*8+1+(oy or 0) print(t,x+1,y+1,sc,fw,s) print(t,x,y,c,fw,s) end
	--[[ printgsc - print grid/shadow/centered 010 ]] local function printgsc(tx,x,y,c,sc,fw,s) fw,s,sc=fw or false,s or 1,sc or 1 if not x then x=(240//8)//2-(txtw(tx)//8)//2 end if not y then y=(136//8)//2 end print(tx,x*8+1,y*8+1,sc,fw,s) print(tx,x*8,y*8,c,fw,s) end
	--[[ printo - print with outline           003 ]] local function printo(tx,x,y,c,oc,fw,s,sf) fw,s,oc,sf=fw or false,s or 1,oc or 1,sf or false for j=y-1,y+1 do for i=x-1,x+1 do print(tx,i,j,oc,fw,s,sf) end end print(tx,x,y,c,fw,s,sf) end

	--[[ string.split - split string at 'sp'   001 ]] local _DEF_PAT,_L_PAT,_R_PAT='([^, ]+)','([^',']+)'function string.split(s,sp) local t={} if sp=="" then for i=1,#s do t[#t+1]=s:sub(i,i) end else sp=sp and _L_PAT..sp.._R_PAT or _DEF_PAT for word in s:gmatch(sp)do t[#t+1]=word end end return t end

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
	--[[ Bresenham Stuff                       005 ]]
		local Bres={}
		Bres={
			_p=function(a,x,y)a[#a+1]={x=x,y=y}end,
			line=function(x1,y1,x2,y2,exclude_start)
				exclude_start=exclude_start or false
				local pts,p,dtx,dty={},Bres._p,x2-x1,y2-y1
				local ix,iy=dtx>0 and 1or-1,dty>0 and 1or-1
				dtx,dty=2*abs(dtx),2*abs(dty)
				if not exclude_start then
					p(pts,x1,y1)
				end
				if dtx>=dty then
					err=dty-dtx/2
					while x1~=x2 do
						if err>0or(err==0 and ix>0)then
							err,y1=err-dtx,y1+iy
						end
						err,x1=err+dty,x1+ix
						p(pts,x1,y1)
					end
				else
					err=dtx-dty/2
					while y1~=y2 do
						if err>0or(err==0 and iy>0)then
							err,x1=err-dty,x1+ix
						end
						err,y1=err+dtx,y1+iy
						p(pts,x1,y1)
					end
				end
				return pts
			end,
			-- not working properly
			ellipse=function(x0,y0,x1,y1)
				-- from: http://members.chello.at/easyfilter/bresenham.html
				local pts,p,a,b={},Bres._p,abs(x1-x0),abs(y1-y0) -- values of diameter
				local b1=b&1
				local dx,dy=4*(1-a)*b*b,4*(b1+1)*a*a -- error increment
				local err=dx+dy+b1*a*a
				local e2 -- error of 1.step

				if x0>x1 then x0,x1=x1,x1+a end -- if called with swapped points
				if y0>y1 then y0=y1 end -- exchange them
				y0=y0+(b+1)//2
				y1=y0-b1   -- starting pixel
				a,b1=a*(8*a),8*b*b

				repeat
					p(pts,x1,y0) --   I. Quadrant
					p(pts,x0,y0) --  II. Quadrant
					p(pts,x0,y1) -- III. Quadrant
					p(pts,x1,y1) --  IV. Quadrant
					e2=2*err
					if e2<=dy then  -- y step
						y0,y1,dy=y0+1,y1-1,dy+a
						err=err+dy
					end
					if e2>=dx or 2*err>dy then  -- x step
						x0,x1,dx=x0+1,x1-1,dx+b1
						err=err+dx
					end
				until x0>x1

				while y0-y1<b do
					-- too early stop of flat ellipses a=1
					p(pts,x0-1,y0) -- -> finish tip of ellipse
					p(pts,x1+1,y0)
					p(pts,x0-1,y1)
					p(pts,x1+1,y1)
					y0,y1=y0+1,y1-1
			   end
			   return pts
			end,
			circle_f=function(xc,yc,r)
				-- filled circle
				-- from: http://members.chello.at/~easyfilter/bresenham.html
				local pts,p,x,y,err={},Bres._p,-r,0,1-2*r-- 2-2*r
				repeat
					for i=xc+x,xc do p(pts,i,yc-y)end
					for i=xc+y,xc,-1 do p(pts,i,yc+x)end
					for i=xc-y,xc do p(pts,i,yc-x)end
					for i=xc-x,xc,-1 do p(pts,i,yc+y)end

					r=err
					if r<=y then
						y=y+1
						err=err+(y*2+1)
					end
					if r>x or err>y then
						x=x+1
						err=err+(x*2+1)
					end
				until x>=0
				return pts
			end,
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
		local _NID,_NIT,_NOK="NO_ID","NO_ITEM","NO_KEY"
		local ui={
			visible=true,
			active=true,
			locked=false,
			mouse_on_ui=false,
			info_item=nil,
			_rend_steps={},
			_items={},
			_curr={hovd=_NID,prsd=_NID},
			_prev={hovd=_NID,prsd=_NID},
			_cache={i={},h={},p={}},
		}

		function ui._push(it)ui._items[#ui._items+1]=it end
		function ui._pop()ui._items[#ui._items]=nil end
		function ui._peek()return ui._items[#ui._items]end

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

		function ui._cache_item(it)
			if ui._cache.i[it.id]then ui._cache.i[it.id]=nil end
		end

		function ui._is_cached_hovd(id)return ui._cache.h[id]~=nil end
		function ui._is_cached_prsd(id)return ui._cache.p[id]~=nil end

		function ui._cache_hovd(id)ui._cache.h[id]=true end
		function ui._cache_prsd(id)ui._cache.p[id]=true end

		function ui._uncache_hovd(id)ui._cache.h[id]=nil end
		function ui._uncache_prsd(id)ui._cache.p[id]=nil end

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

		function ui.start_frame()
			ui.mouse_on_ui=false

			if ui.info_item and not ui._cache.h[ui.info_item.id]then
				ui.info_item=nil
			end
		end

		function ui._render()
			local unpk,_rend_step_fns=unpk,ui._rend_step_fns
			for _,v in ipairs(ui._rend_steps)do
				_rend_step_fns[v[1]](unpk(v[2]))
			end
			ui._rend_steps={}
		end

		function ui.end_frame()
			ui._render()

			if not m1 then
				ui._curr.prsd=_NID
				ui._prev.prsd=_NID
			else
				if ui._curr.prsd==_NID then
					ui._curr.prsd=_NIT
				end
			end

			if ui._curr.hovd==_NID then
				ui._prev.hovd=_NID
			end

			---- HOUSEKEEPING ----
			for id,_ in pairs(ui._cache.i)do
				if ui._cache.h[id]then ui._cache.h[id]=nil end
				if ui._cache.p[id]then ui._cache.p[id]=nil end
			end
			ui._cache.i=dmerge(ui._cache.h,ui._cache.p)
		end

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

		function Item.exec(t)
			if t.code then t.code(t,t.args)end
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
						if t.tip then ui.info_item=t end
						if not t:_is_last_hovered() then
							t.mouse_entered=true
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

	--[[ TICkle Extensions                      007 ]]
		--[[ print with shadow rendering            001 ]]
			ui.add_render_step("prints",
				function(...)ui.push_render_step("prints",{...})end,
				prints
			)
		--[[ print with outline                     001 ]]
			ui.add_render_step("printo",
				function(...)ui.push_render_step("printo",{...})end,
				printo
			)
		--[[ tiled1 rendering (from 3x1 sprs)       005 ]]
			ui.add_render_step("tiled1",
				function(...)ui.push_render_step("tiled1",{...})end,
				function(idx,x,y,w,h,ac)
					local i1,i2,flp,tw,th=idx+1,idx+2,false,w//8,h//8
					clip(x+8,y+8,w-16,h-16)-- fill background
						for j=0,th do
							for i=0,tw do
								spr(i2,x+i*8, y+j*8,ac,1,flp,0)
							end
						end
					clip()
					clip(x+8,y,w-16+1,h)-- horizontal borders
						for i=0,tw do
							spr(i1,x+i*8,y,ac,1,flp,0)
							spr(i1,x+i*8,y+h-8,ac,1,flp,2)
						end
					clip()
					clip(x,y+8,w,h-16+1)-- vertical borders
						for j=0,th do
							spr(i1,x,y+j*8,ac,1,flp,3)
							spr(i1,x+w-8,y+j*8,ac,1,flp,1)
						end
					clip()
					clip(x,y,w,h)-- corners
						spr(idx,x,y,ac,1,flp,0)  -- tl
						spr(idx,x+w-8,y,ac,1,flp,1)  -- tr
						spr(idx,x+w-8,y+h-8,ac,1,flp,2)  -- br
						spr(idx,x,y+h-8,ac,1,flp,3)  -- br
					clip()
				end
			)
--=--=--=--=--=--=--=--=--=--=--=--=--
-- setup
	-- in-game persistent options / load values or defaults
	local webv=false
	local opts,PALM={true,true,true,false,(webv and 2 or 1),[9]=true},0x03FC0
	local USE_PADDING,WRAP_AROUND,RAND_START,RAND_RESET,ZOOM_LVL,FG_R,FG_G,FG_B,USE_TLTIPS,BG_R,BG_G,BG_B=1,2,3,4,5,6,7,8,9,10,11,12

	-- original cell color = da7100 | 218,113,0
	local cell_col=webv and{peek(PALM+8*3),peek(PALM+8*3+1),peek(PALM+8*3+2)}or nil
	local bg_col=webv and{peek(PALM+15*3),peek(PALM+15*3+1),peek(PALM+15*3+2)}or nil

	function set_cell_color(c)
		cell_col=c
		set_color(8,c,FG_R,FG_G,FG_B)
	end
	function set_bg_color(c)
		bg_col=c
		set_color(15,c,BG_R,BG_G,BG_B)
	end

	function set_color(idx,c,ri,gi,bi)
		local i,r,g,b=PALM+idx*3,unpk(c)
		poke(i  ,r)
		poke(i+1,g)
		poke(i+2,b)
		if not webv then
			pmem(ri,r)
			pmem(gi,g)
			pmem(bi,b)
		end
	end

	if not webv then
		if pmem(USE_PADDING) ~= 1 then opts[USE_PADDING] = false end
		if pmem(WRAP_AROUND) ~= 1 then opts[WRAP_AROUND] = false end
		if pmem(RAND_START)  ~= 1 then opts[RAND_START]  = false end
		if pmem(RAND_RESET)  ~= 0 then opts[RAND_RESET]  = true end
		if pmem(USE_TLTIPS)  ~= 1 then opts[USE_TLTIPS]  = false end --tooltip
		if pmem(ZOOM_LVL)==0 then pmem(ZOOM_LVL, opts[ZOOM_LVL]) end

		local mr,mg,mb=pmem(FG_R),pmem(FG_G),pmem(FG_B)
		set_cell_color({mr,mg,mb})

		mr,mg,mb=pmem(BG_R),pmem(BG_G),pmem(BG_B)
		set_bg_color({mr,mg,mb})
	end

	function toggle_opt(i) -- toggle value of an option and save it
		local b=not opts[i]
		opts[i]=b
		if not webv then pmem(i,b and 1 or 0)end
		if i==USE_PADDING then set_padding(b)end
	end

	local upd_delay=0
	local g_mx,g_my,g_lmx,g_lmy=0,0,0,0 -- grid mouse pos
	local ctrl,shift,alt=false,false,false

	local pre,cur,dum,sav=1,2,3,4 -- prev/curr/dummy,saved buffer indices
	local cells={} -- cell buffers
	local CS,GW,GH=8//opts[ZOOM_LVL], 30*opts[ZOOM_LVL], 17*opts[ZOOM_LVL] -- cell size, grid width/height
	local zoom_mults={8,4,2,1}
	local pad=0 -- padding for cell rects (always 0 if 'opts[ZOOM_LVL] < 8' -- see 'set_padding()')
	local paused,stopped=true,true
	local l_cells,gens,TOT_CELLS=0,0,0 -- living cells / generations
	local NUM_HELP_SCRS,state=4,"game"

	local cats={
		"Statics",
		"Oscilators",
		"Amusing",
		"Spaceships",
		"Guns",
	}

	-- help strings
	--------------------------
	local help_strs={
		[[

		                1 Generation count

		   1    2       2 Playback controls

		                3 Drawing tools
		 3
		                4 Pattern category/name

		      4         5 Mouse position

		                6 Zoom level
		   5 6 7   8
		                7 Speed meter

		                8 Cells alive|dead/total
		]],
		[[
			H/O       Show help/options
			SPACE     Pause/play
			  +SHIFT  Stop (reset)
			ENTER     Randomize board
			SHIFT+C/F Clear/fill board
			G         Next generation (if paused)
			TAB/I/T   Toggle UI/info/toolbars
			PG-UP/DN  Zoom in/out (clears board)
			Up/Down   inc/decrease speed by 1%
			  +SHIFT  fast inc/decrease speed
			P         Toggle padding (if zoom>2)
			Ctrl+S/L  Save/load board state
			Q/E       Rotate pattern|clipboard
			A/Z       Flip pattern|clipboard h/v
		]],
		[[
			Mouse1(L) Drawing/cancel erasing
			Mouse2(R) Erase/cancel drawing
		]],
		[[
			B/D/1     Brush tool (x2 switch mode)
			2-6       Pattern tool/categories
			R/C       Rectangle/circle tools
			L         Line tool
			F         Fill tool
			Ctrl+C    Copy tool
			Ctrl+X    Cut tool
			Ctrl+V    Paste tool
		]],
		[[
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
			5         Spaceships
			6         Guns
		]]
	}
--=--=--=--=--=--=--=--=--=--=--=--=--

-- fwd decls
local tl,rand_cells

local function inbounds(x,y)
	return x>0 and x<=GW and y>0 and y<=GH
end


--=--=--=--=--=--=--=--=--=--=--=--=--
-- GUI
	local pb_rect={x=240//2-76//2,  y=-2,             w=76,  h=10}
	local tb_rect={x=-2,            y=136//2-100//2,  w=10,  h=100}
	local thm={
		bg=15,
		fg=13,
		text=5,
		shad=3,
		outl=1,
		header=9,
		cell=8,
		preview=11,
		select=14,
		erase=7,
		dim_text=14,
	}

	local tb_vis,info_vis=true,true
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
			t:exec()
			ui.spr(_btn_icon(t,icn),t.gx,t.gy,0)
		end)
	end

	function TLButton(id,x,y,icn,on,op)
		return ui.with_item(id,x,y,8,8,op,function(t,...)
			t:check_hovered(t.gx,t.gy,t.w,t.h)
			t:check_pressed()
			t:exec()
			icn=(not ui.active and icn+4) or on and icn+2 or icn
			ui.spr(icn+(t.hovered and 1 or 0),t.gx,t.gy,0)
		end)
	end

	local tooltip={id=nil,timer=0,delay=1}
	function ToolTip()
		local i,t,oid,tw=ui.info_item,tooltip
		oid,t.i=t.i and t.i.id or nil,i
		if i then
			if i.id~=oid or i.pressed then t.timer=t.delay
			elseif t.timer>0          then t.timer=t.timer-dt
			else
				tw=txtw(i.tip,true,_,true)
				ui.rect(mx+2,my+4,tw,8,5)
				ui.print(i.tip,mx+4,my+5,1,false,1,true)
			end
		end
	end

	function GenInfo()
		if ui.visible then
			local ty,tc,oc,fw,sf,cstr,tcstr,tstr,txt,tw=2,thm.text,thm.outl,true,true,tostr(l_cells),tostr(TOT_CELLS-l_cells),tostr(TOT_CELLS)
			printo("Gen: "..gens,2,ty,tc,oc,fw,_,sf)

			ty=ty+8*15-1
			if tl.type=="patt"then
				txt=fmt("%s: %s",cats[tl.cur_cat],tl.patts[tl.cur_cat][tl.cur_patt].name)
				tw=txtw(txt,fw,_,sf)
				printo(txt,120-tw//2,ty-2,thm.dim_text,oc,fw,_,sf)
			end
			ty=ty+8
			printo(fmt("%s, %s",g_mx-1,g_my-1),2,ty,thm.dim_text,oc,fw,_,sf)
			printo(fmt("Zoom:%s",opts[ZOOM_LVL]),45,ty,tc,oc,fw,_,sf)
			printo("Speed:"..1-(1*(upd_delay/100)),83,ty,tc,oc,fw,_,sf)

			printo("Cells:"..rep(' ',6-#cstr)..cstr
				 .."|"..rep(' ',6-#tcstr)..tcstr
				 .."/"..rep(' ',6-#tstr)..tstr
				,135,ty,tc,oc,fw,_,sf)
		end
	end

	function Separator(x,y)
		ui.with_item("-",x,y,0,0,_,function(t)
			ui.spr(5,t.gx,t.gy,0)
		end)
	end

	function PlaybackBar(id,r,op)
		ui.with_item("pb",r.x,r.y,r.w,r.h,op,function(t)
			ui.tiled1(10,r.x,r.y,r.w,r.h,0)
			local b1,b2,b3,b4,b5,b6,b7,b8,b9

			b1=Button("b_rand",2,1,16, {tip="Randomize cells"}   )
			b2=Button("b_zoom",10,1,19, {tip="Zoom in"}   )
			Separator(15,1)
			b3=ui.with_active(upd_delay<100,Button,"b_back",22,1,32,{tip="Slower speed"})
			b4=Button("b_stop",30,1,stopped and 67 or 64,{tip="Stop and reset board"})

			if stopped then
				b5=Button("b_play",38,1,80,{tip="Start simulation"})
				if b5.released then unpause()end
			elseif paused then
				b5=Button("b_play",38,1,83,{tip="Resume simulation"})
				if b5.released then unpause()end
			else
				b6=Button("b_pause",38,1,96,{tip="Pause simulation"})
				if b6.released then pause()end
			end

			b7=ui.with_active(upd_delay>0,Button,"b_fwd",46,1,48,{tip="Faster speed"})

			Separator(52,1)
			b8=Button("b_opts",59,1,240,{tip="Show options"})
			b9=Button("b_help",67,1,243,{tip="Show help"})

			if b1.released then rand_cells(opts[RAND_RESET])end
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

	local tl_types={"brush","rect","circ","line","fill","patt","copy","cut","paste"}
	function Toolbar(id,r,op)
		ui.with_item("tb",r.x,r.y,r.w,r.h,op,function(t)
			ui.tiled1(10,r.x,r.y,r.w,r.h,0)
			local ttp,cb,brt,btns=tl.type,#tl.clipboard>0,tl.brush_mode
			btns={
					TLButton("b_brush",1,1,112,ttp=="brush",{tip="Brush tool"}),
					TLButton("b_rect",1,9,128,ttp=="rect",{tip="Rect tool"}),
					TLButton("b_circ",1,17,144,ttp=="circ",{tip="Circle tool"}),
					TLButton("b_line",1,25,160,ttp=="line",{tip="Line tool"}),
					TLButton("b_fill",1,33,176,ttp=="fill",{tip="Fill tool"}),
					TLButton("b_patt",1,41,192,ttp=="patt",{tip="Pattern tool"}),
					TLButton("b_copy",1,49,136,ttp=="copy",{tip="Copy tool"}),
					TLButton("b_cut",1,57,152,ttp=="cut",{tip="Cut tool"})
				}
			btns[9]=ui.with_active(cb,TLButton,"b_paste",1,65,168,ttp=="paste",{tip="Paste tool"})
			for i,b in ipairs(btns)do
				if b.pressed then tl:switch(tl_types[i])end
			end
			ui.spr(4,t.gx+1,t.gy+71,0)
			if ttp=="brush"then
				ui.spr(brt=="round"and 87 or 86,t.gx+1,t.gy+76,0)
				ui.spr(brt=="square"and 103 or 102,t.gx+1,t.gy+83,0)
			elseif ttp=="rect"then
				ui.spr(shift and 119 or 118,t.gx+1,t.gy+76,0)
				ui.spr(ctrl and 135 or 134,t.gx+1,t.gy+83,0)
				ui.spr(alt and 151 or 150,t.gx+1,t.gy+90,0)
			elseif ttp=="circ"then
				ui.spr(shift and 167 or 166,t.gx+1,t.gy+76,0)
				ui.spr(ctrl and 183 or 182,t.gx+1,t.gy+83,0)
				ui.spr(alt and 199 or 198,t.gx+1,t.gy+90,0)
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
				t.val=val
			end
			t:exec()
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
			t:exec()
			local handle=t.hovered and(is_on and 248 or 246)
									or(is_on and 232 or 230)
			ui.spr(handle,t.gx,t.gy,-1,1,0,0,2,1)
		end)
	end

	function Slider(id,x,y,l,val,minv,maxv,op)
		local w,h,ov,px,py=l*8,8,val
		ui.with_item(id,x,y,w,h,_,function(t,...)
			px,py=t.gx,t.gy
			t:check_hovered(px,py,w,h)
			t:check_pressed()

			if t.held then
				pmx = clamp(mx-px,0,w)
				val=(pmx*maxv)//(w)
			end

			if val~=ov then t.val_changed=true end
			t.val=val
			t:exec()
			for i=0,(l-1)*8,8 do
				ui.spr(7,px+i,py)
			end
			px=px+clamp(((w-2)*val)//maxv,0,(w-2))
			ui.spr(9,px,py,0)
		end)
		return val
	end

	function ColorBar(id,x,y,val)
		local w,h,b1,b2=10*8,8
		ui.with_item(id,x,y,w,h,_,function(t, ...)
			val=Slider("s",8,0,8,val,0,255)
			ui.spr(6,t.gx,t.gy)
			ui.spr(8,t.gx+9*8,t.gy)
			b1=ui.with_active(val>0,Button,"lb",0,0,22)
			b2=ui.with_active(val<255,Button,"rb",9*8,0,26)
			if b1.released then val=val-1 end
			if b2.released then val=val+1 end
		end)
		return val
	end

	function ColorPicker(id,x,y,name,s_idx,c)
		local w,h,tc,r,g,b=80,24,thm.dim_text,unpk(c)
		ui.with_item(id,x,y,w,h,_,function(t,...)
			Label("l4",16,1,name,thm.txt,{shadow=1})
			ui.spr(s_idx,t.gx+88,t.gy,0)

			r=ColorBar("cb1",8,8,r)
			g=ColorBar("cb2",8,16,g)
			b=ColorBar("cb3",8,24,b)

			Label("l1",8*11,8+1, r,tc)
			Label("l1",8*11,16+1,g,tc)
			Label("l1",8*11,24+1,b,tc)
		end)
		return {r,g,b}
	end
--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--
-- Flood Fill
	-- TODO: revise this
	local function _has_pix(v,x,y,s,c,tmp)
		if tmp then
			return x<1 or y<1 or x>GW or y>GH
			or s[y][x]==1
		else
			return x<1 or y<1 or x>GW or y>GH
			or s[y][x]==1 or c[y][x]==v
		end
	end

	--   Scanline FT
	local function scnln_ft(x, y, tmp,v)
		local pts={}
		local function _p(x,y)
			if not x then
				local p=pts[#pts]
				pts[#pts]=nil
				return p
			end
			pts[#pts+1]={x=x,y=y}
		end

		_p(x,y) -- add initial point

		local s,c,pt,set_abv,set_blw,sy=cells[dum],cells[cur]
		repeat
			pt = _p()
			set_abv,set_blw,sy,x=true,true,s[pt.y],pt.x
			while not _has_pix(v,x,pt.y,s,c,tmp)do
				sy[x]=1
				if _has_pix(v,x,pt.y-1,s,c,tmp)~=set_abv then
					set_abv=not set_abv
					if not set_abv then _p(x,pt.y-1)end
				end
				if _has_pix(v,x,pt.y+1,s,c,tmp)~=set_blw then
					set_blw=not set_blw
					if not set_blw then _p(x,pt.y+1)end
				end
				x=x+1
			end
			set_abv=pt.y>0 and _has_pix(v,pt.x,pt.y-1,s,c,tmp)
			set_blw=pt.y<GH-1 and _has_pix(v,pt.x,pt.y+1,s,c,tmp)
			x=pt.x-1
			while not _has_pix(v,x,pt.y,s,c,tmp)do
				sy[x]=1
				if _has_pix(v,x,pt.y-1,s,c,tmp)~=set_abv then
					set_abv=not set_abv
					if not set_abv then _p(x,pt.y-1)end
				end
				if _has_pix(v,x,pt.y+1,s,c,tmp)~=set_blw then
					set_blw=not set_blw
					if not set_blw then _p(x,pt.y+1)end
				end
				x=x-1
			end
		until #pts==0
	end

	local function flood_fill(x,y,tmp)
		scnln_ft(x,y,tmp or false,tl.mode=="draw"and 1 or 0)
	end
--=--=--=--=--=--=--=--=--=--=--=--=--



--=--=--=--=--=--=--=--=--=--=--=--=--
-- Geometry stuff

	local geom={}
	geom={
		p=function(x,y)return{x=x,y=y}end,
		--TODO: these functiosn shouldn't draw anything,
		--      they should just return a list of points
		_rect_hollow=function(r)
			local t,p={},geom.p
			for j=r.y,r.y2 do
				t[#t+1]=p(r.x,j)
				t[#t+1]=p(r.x2,j)
			end
			for i=r.x,r.x2 do
				t[#t+1]=p(i,r.y)
				t[#t+1]=p(i,r.y2)
			end
			return t
		end,
		_rect_filled=function(r)
			local t,p={},geom.p
			for j=r.y,r.y2 do
				for i=r.x,r.x2 do
					t[#t+1]=p(i,j)
				end
			end
			return t
		end,
		_circle_filled=function(x,y,x1,y1,x2,y2,r)
			local t,p,R={},geom.p,r*r
			for j=y-r,y+r do
				for i=x-r,x+r do
					if sdist(i,j,x,y)<=R+1 then
						t[#t+1]=p(i,j)
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
		origin=nil,
		w=0,h=0,
		mode=nil,
		mod1=false,  -- filled
		mod2=false,  -- square
		mod3=false,  -- centered on origin
		brush_size=0,
		brush_mode="round",
		max_size=10,
		patts={},
		cur_patts={},
		cur_cat=1,
		cur_patt=1,
		do_update=false,
		clipboard={},
		info=nil,
	}
	function tl.start(t,erase)
		t.origin=vec2(g_mx,g_my)
		t.mode=erase and"erase"or"draw"
		t.do_update=true
	end

	function tl.stop(t)
		t.mode=nil
		t.do_update=true
	end

	function tl.copy(t)
		local cb,c,r,cj,cbj={},cells[cur],tl:_base_rect(g_mx,g_my)
		for j=0,r.h-1 do
			cbj,cj={},c[j+r.y]
			for i=0,r.w-1 do
				cbj[i]=cj[i+r.x]
			end
			cb[j]=cbj
		end
		t.clipboard=cb
		if tl.type=="cut" then
			for j=0,r.h-1 do
				cj=c[j+r.y]
				for i=0,r.w-1 do
					cj[i+r.x]=0
				end
			end
		end
		t:clear()
		t:stop()
	end

	function tl.commit(t,cancel)
		if not t.mode then return end
		if tl.type~="copy"and tl.type~="cut"then
			local s,c,v,sj,cj=cells[dum],cells[cur],t.mode=="draw"and 1 or 0
			for j=1,GH do
				sj,cj=s[j],c[j]
				for i=1,GW do
					if sj[i]==1then
						cj[i]=v
					end
				end
			end
			if opts[WRAP_AROUND]then swap_borders(c)end
			if cancel then t:stop()end
			t.do_update=true
			t.info=nil
		end
	end

	function tl.clear(t)
		local s,sj=cells[dum]
		for j=1,GH do
			sj=s[j]
			for i=1,GW do
				sj[i]=0
			end
		end
	end

	function tl.switch(t,tp,force)
		if t.type~=tp or force then
			t:stop()
			t.type=tp
			t.do_update=true
		end
	end

	function tl.toggle_brush(t)
		t.brush_mode=t.brush_mode=="square"and"round"or"square"
		t.do_update=true
	end

	function tl.set_mods(c,s,a)
		if tl.type=="rect"or tl.type=="circ"then
			if tl.mod1~=c or tl.mod2~=s or tl.mod3~=a then tl.do_update=true end
			tl.mod1,tl.mod2,tl.mod3=c,s,a
		end
	end

	function tl.expand(t)
		t.brush_size=min(t.brush_size+1, t.max_size)
		t.do_update=true
	end

	function tl.contract(t)
		t.brush_size=max(t.brush_size-1, 0)
		t.do_update=true
	end

	function tl._brush_pts(t,x,y)
		local path=t.mode and Bres.line(g_lmx,g_lmy,x,y) or {geom.p(x,y)}
		local set,pts,R,p,xmin,ymin,xmax,ymax={},{},t.brush_size
		for i=1,#path do
			p=path[i]
			xmin,ymin,xmax,ymax=max(1,p.x-R),max(1,p.y-R),min(GW,p.x+R),min(GH,p.y+R)

			if t.brush_mode=="square"then pts=geom._rect_filled(rec4(xmin,ymin,xmax-xmin,ymax-ymin))
			elseif t.brush_mode=="round"then pts=Bres.circle_f(p.x,p.y,R)
			end

			for i=1,#pts do
				set[pts[i]]=1
			end
		end

		pts={}
		for k,_ in pairs(set)do
			pts[#pts+1]=k
		end

		tl._commit_pts(pts)
	end

	function tl._commit_pts(pts)
		local s=cells[dum]
		for _,p in ipairs(pts)do
			if inbounds(p.x,p.y)then
				s[p.y][p.x]=1
			end
		end
	end

	function tl._base_rect(t,x,y)
		local o,p1,p2,s,r=t.origin,{}
		p1,p2=vec2(min(x,o.x),min(y,o.y)),vec2(max(x,o.x),max(y,o.y))
		s=p2-p1
		if t.mod2 then -- if square
			s=vec2(min(s.x,s.y),min(s.x,s.y))
			if x<o.x then p1.x=p1.x+(o.x-(p1.x+s.x))end
			if y<o.y then p1.y=p1.y+(o.y-(p1.y+s.y))end
		end
		return rec2(p1,s)
	end

	function tl._chk_centr(t,r)
		if t.mod3 then
			r.p=t.origin-r.s//2
			r.s=vec2(floor(r.s.x*1.5),floor(r.s.y*1.5))
		end
		return r
	end

	function tl.show_info(t)
		local w,h,x,y=txtw(t.info,true,1,true),8
		x=clamp(mx+2,0,240-w)
		y=clamp(my-6,0,136-h)
		ui.printo(t.info,x,y,thm.dim_text,thm.outl,true,1,true)
	end

	function tl._rect_pts(t,x,y)
		if t.mode then
			local r,pts=t:_chk_centr(t:_base_rect(x,y))
			if t.mod1 then pts=geom._rect_filled(r)
			else           pts=geom._rect_hollow(r)
			end
			t._commit_pts(pts)
			t.info = (r.w+1)..","..(r.h+1)
		end
	end

	function tl._circle_pts(t,x,y)
		if t.mode then
			local r,pts,hw,hh=t:_chk_centr(t:_base_rect(x,y))
			hw,hh=r.w//2,r.h//2
			pts=Bres.ellipse(r.x,r.y,r.x+r.w,r.y+r.h)
			t._commit_pts(pts)
			if t.mod1 then flood_fill(r.c.x,r.c.y,true)end
			t.info = (r.w+1)..","..(r.h+1)
		end
	end

	function tl._select_pts(t,x,y)
		if t.mode then
			tl._commit_pts(geom._rect_hollow(tl:_base_rect(x,y)))
		end
	end

	function tl._line_pts(t,x,y)
		if t.mode then
			local ox,oy,w,h=t.origin.x,t.origin.y
			w,h=ox-x,oy-y
			tl._commit_pts(Bres.line(ox,oy,x,y))
			t.info = ""..floor(sqrt(sdist(ox,oy,x,y)))+1
		end
	end

	function tl._fill_pts(t,x,y)
		t.mode=cells[cur][y][x]>0 and"erase"or"draw"
		flood_fill(x,y)
	end

	function tl._patt_pts(t,x,y)
		local s,p,gy,sj,pj,gx=cells[dum],t.patts[t.cur_cat][t.cur_patt].layout
		local w,h=#p[1],#p
		for j=1,h do
			gy=j+(y-1)-h
			if gy>0 and gy<=GH then
				sj,pj=s[gy],p[j]
				for i=1,w do
					gx=i+(x-1)-w
					if gx>0 and gx<=GW then
						sj[gx]=pj[i]
					end
				end
			end
		end
	end

	function tl.rot(t,n)
		local a
		if tl.type=="patt"then
			a=t.patts[t.cur_cat][t.cur_patt].layout
			t.patts[t.cur_cat][t.cur_patt].layout=rotate(a,n)
		elseif tl.type=="paste"then
			a=t.clipboard
			t.clipboard=rotate(a,n)
		end
		if a then t.do_update=true end
	end
	function tl.flip(t,d)
		local a
		if tl.type=="patt"then
			a=t.patts[t.cur_cat][t.cur_patt].layout
			t.patts[t.cur_cat][t.cur_patt].layout=d=="v"and vflip(a)or hflip(a)
		elseif tl.type=="paste"then
			a=t.clipboard
			t.clipboard=d=="v"and vflip(a)or hflip(a)
		end
		if a then t.do_update=true end
	end

	function tl._paste_pts(t,x,y)
		local s,p,gy,sj,pj,gx=cells[dum],t.clipboard
		local w,h=#p[1],#p
		for j=1,h do
			gy=j+(y-1)-h
			if gy>0 and gy<=GH then
				sj,pj=s[gy],p[j]
				for i=1,w do
					gx=i+(x-1)-w
					if gx>0 and gx<=GW then
						sj[gx]=pj[i]
					end
				end
			end
		end
	end

	tl._draw_fns={
		brush = tl._brush_pts,
		rect  = tl._rect_pts,
		circ  = tl._circle_pts,
		line  = tl._line_pts,
		fill  = tl._fill_pts,
		patt  = tl._patt_pts,
		copy  = tl._select_pts,
		cut   = tl._select_pts,
		paste = tl._paste_pts,
	}

	function tl.draw_points(t,x,y)
		if t.do_update then
			tl:clear()
			t._draw_fns[t.type](t,x, y)
			t.do_update=false
		end
	end

	function tl.set_category(t,c)
		t.cur_cat = c
		t.cur_patt = t.cur_patts[t.cur_cat]
		t.do_update=true
	end

	function tl.next_pattern(t)
		local cat=t.cur_cat
		local opat, npat=t.cur_patt, clamp(t.cur_patts[cat]+1, 1, #t.patts[cat])
		if npat ~= opat then
			t.cur_patt = npat
			t.cur_patts[cat] = npat
		end
		t.do_update=true
	end

	function tl.prev_pattern(t)
		local opat, npat = t.cur_patt, clamp(t.cur_patts[t.cur_cat]-1, 1, #t.patts[t.cur_cat])
		if npat ~= opat then
			t.cur_patt = npat
			t.cur_patts[t.cur_cat] = npat
		end
		t.do_update=true
	end

	function tl.init_pats(t)
		local pats={
			{ -- statics
				"4 3 Beehive .2o o2.o .2o",
				"4 4 Loaf .2o o2.o o.o .o",
				"3 3 Boat 2o o.o .o",
				"3 3 Ship 2o o.o .2o",
			},
			{ -- oscilators
				"4 2 Toad .3o 3o",
				"3 10 Pentadecathlon .o .o o.o .o .o .o .o o.o .o .o",
				"4 4 ? 2o o 3.o 2.2o",
				"5 4 Clock 2.o 2o 2.2o .o",
				"13 13 Pulsar 2.3o3.3o . o4.o.o4.o o4.o.o4.o o4.o.o4.o 2.3o3.3o . 2.3o3.3o o4.o.o4.o o4.o.o4.o o4.o.o4.o . 2.3o3.3o",
				"15 18 ? 8.2o2.2o 8.o4.o 9.4o 6.3o3.o.o 2.o3.o2.o3.2o .o.o3.o.o o.o2.o.o.2o o2.2o.o3.o .o.o2.o.2o 2o.o.o.o2.o o2.o.o.o.2o .2o6.o 3.5o.o 3.o4.o 4.4o . 4.2o 4.2o",
			},
			{ -- amusing
				"3 3 R_Pentomino .2o 2o .o",
				"8 2 Diehard 2o4.o .o3.3o",
				"29 10 ? 2o14.2o9.2o 2o15.2o8.2o 13.5o 13.4o12. . 3.2o8.4o 2.o.o.2o5.5o .2o3.2o9.2o8.2o 2.o.o.2o8.2o9.2o 3.2o",
				"39 1 ? 8o.5o3.3o6.7o.5o",
				"7 3 Acorn .o 3.o 2o2.3o",
				"5 5 ? 3o.o o 3.2o .2o.o o.o.o",
				"8 6 ? 6.o 4.o.2o 4.o.o 4.o 2.o o.o",
			},
			{ -- Spaceships
				"3 3 Glider 2.o o.o .2o",
				"5 4 Lightweight_Spaceship o2.o 4.o o3.o .4o",
				"12 17 Brain 2.2o .o2.o5.2o .3o3.3o .o2.o.4o 2.3o3.o 2.o2.3o 4.o4.2o 4.5o.o . 4.5o.o 4.o4.2o 2.o2.3o 2.3o3.o .o2.o.4o .3o3.3o .o2.o5.2o 2.2o",
				"31 17 Blinker_Ship 12.o2.o 11.o 11.o3.o 3.2o6.4o 2.4o .2o.2o 2.2o5.2o.3o 8.o5.2o7.o4.3o 7.2o7.o6.o4.o.o 8.o5.2o7.o4.3o 2.2o5.2o.3o .2o.2o 2.4o 3.2o6.4o 11.o3.o 11.o 12.o2.o",
			},
			{ -- guns
				"36 9 Glider_Gun 24.o11. 22.o.o 12.2o6.2o12.2o 11.o3.o4.2o12.2o 2o8.o5.o3.2o 2o8.o3.o.2o4.o.o 10.o5.o7.o 11.o3.o 12.2o",
				"56 1 1D_Four_Gliders 56o",
				"60 56 Glider_Puffer 40.3o 39.o2.o 42.o4.3o 42.o4.o3.o4.o 39.o.o4.o3.o3.3o 46.4o4.o.2o 47.o7.3o 55.3o 41.o13.2o 40.3o 39.2o.o 39.3o 40.2o . . . . . . 39.o 38.3o 37.2o.o11.3o 37.3o11.o2.o 37.3o14.o 38.2o14.o 51.o.o . . 38.o 39.o 34.o4.o9.o 35.5o10.o6.2o 46.o3.o4.2o.2o 47.4o4.4o 56.2o . . 20.2o33.o o2.o13.3o.2o32.2o 4.o12.5o34.2o o3.o13.3o34.2o .4o . 36.2o 21.3o10.2o.2o 23.o10.4o13.5o 21.o.o11.2o13.o4.o 21.2o32.o 54.o . 14.2o5.o2.o 13.4o8.o 2.3o8.2o.2o3.o3.o .5o9.2o5.4o .3o.2o 4.2o",
			},
		}

		local function add(a,v,n)
			for _=1,n do a[#a+1]=v end
		end

		local function parsenum(i,str)
			local s,nex=str:sub(i,i)
			while i<i+4 do -- inf loop guard
				i=i+1
				nex=str:sub(i,i)
				if nex=="."or nex=="o"then break end
				s=s..nex
			end
			return tonum(s),i
		end

		local function parse_patt(p)
			local lns=p:split(' ')
			local w,h,name,pat,i,c,num,row=tonum(lns[1]),tonum(lns[2]),lns[3],{}
			for i=1,3 do rem(lns,1)end
			for _,l in ipairs(lns) do
				i,row=1,{}
				while i <= w do
					c=l:sub(i,i)
					if c==""then
						add(row,0,w-#row)
						break
					end
					num=tonum(c)
					if not num then
						add(row,c=="o" and 1 or 0,1)
						i=i+1
					else
						num,i=parsenum(i,l)
						c=l:sub(i,i)
						add(row,c=="o" and 1 or 0,num)
						i=i+1
					end
				end
				pat[#pat+1]=row
			end
			return {w=w,h=h,name=name:gsub('_',' '),layout=pat}
		end

		for j,c in ipairs(pats) do
			t.cur_patts[j]=1
			t.patts[j]={}
			for i,p in ipairs(c) do
				t.patts[j][i]=parse_patt(p)
			end
		end
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
		local c,b=cells[cur],0
		for j=1,GH do
			for i=1,GW do
				b=rand()<0.5 and 1 or 0
				c[j][i]=b
				l_cells=l_cells+b
			end
		end
	end

	function create_cells()
		local b1,b2,b3,b4={},{},{},{}
		for j=0,GH+1 do
			b1[j],b2[j],b3[j],b4[j]={},{},{},{}
			for i=0,GW+1 do
				b1[j][i],b2[j][i],b3[j][i],b4[j][i]=0,0,0,0
			end
		end
		cells={b1,b2,b3,b4}
		TOT_CELLS=GW*GH
	end

	function save_board()
		local c,sv=cells[cur],cells[sav]
		for j=1,GH do
			for i=1,GW do
				sv[j][i]=c[j][i]
			end
		end
	end

	function load_board()
		local c,sv=cells[cur],cells[sav]
		for j=1,GH do
			for i=1,GW do
				c[j][i]=sv[j][i]
			end
		end
	end

	function fill_grid(fill)
		local c,v=cells[cur],fill and 1 or 0
		for j=0,GH+1 do
			for i=0,GW+1 do
				c[j][i]=v
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
			if not webv then pmem(ZOOM_LVL,val)end
		end
	end

	function init()
		set_padding(opts[USE_PADDING])
		tl.origin = vec0()
		tl:init_pats()
		-- create_cells()
		set_zoom(pmem(ZOOM_LVL),false,true) -- cells are created here
		if opts[RAND_START] then rand_cells() end
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- update
	function swap_borders(c)
		for j=1,GH do
			c[j][GW+1],c[j][0]=c[j][1],c[j][GW]
		end
		for i=0,GW+1 do
			c[GH+1][i]=c[1][i]
			c[0][i]=c[GH][i]
		end
	end
	-- the weirdness in here seems to have given me over 10ms and 20fps
	local function compute_gen()
		cur,pre=pre,cur
		local lc,p,c=0,cells[pre],cells[cur]	-- count alive cells, and make buffers local
		local l,r,n,b,cj,pj,pu,pd -- this seems to give me ~2ms (and more token budget)

		for j=1,GH do
			cj,pj,pu,pd=c[j],p[j],p[j-1],p[j+1] -- make buffer rows local here for faster access
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

		if opts[WRAP_AROUND]then  -- swap borders
			swap_borders(c)
		end
		l_cells,gens=lc,gens+1
	end

	local function update_ui()
	bma("ui update",function()--@bm
		if state=="game"and tb_vis then
			PlaybackBar("pb", pb_rect)
			Toolbar("tb",tb_rect)
		end
	end)--@bm
	end

	local function update()
	bma("update",function()--@bm
		if state=="game"then
			if mmoved() then
				if tl.type~="copy"or tl.mode then
					tl.do_update=true
				end
			end
			tl:draw_points(g_mx,g_my)
			if tl.info then tl:show_info()end

			bma("comput gen", function()
				if not paused and (upd_delay==0 or f%upd_delay==0) then
					compute_gen()
				end
			end) --@bm
		end
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- render
	local function prevw_color()
		return((tl.mode=="erase" or tl.type=="cut")and thm.erase or(tl.type=="copy"and thm.select) or thm.preview)
	end
	local function render_rects()
		local rect,s,c,rs,ca,cs,sj,cj,x,y=rect,cells[dum],cells[cur],CS-pad,thm.cell,prevw_color()
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
		if not tl.mode and not ui.mouse_on_ui then
			rect((g_mx-1)*CS+pad,(g_my-1)*CS+pad,CS-pad,CS-pad,cs)
		end
	end

	local function render_pix()
		local pix,s,c,ca,cs,sj,cj,y=pix,cells[dum],cells[cur],thm.cell,prevw_color()
		for j=1,GH do
			y,sj,cj=j-1,s[j],c[j]
			for i=1,GW do
				if sj[i]==1 then pix(i-1,y,cs)
				elseif cj[i]==1 then pix(i-1,y,ca)
				end
			end
		end
		if not tl.mode and not ui.mouse_on_ui and tl.type~="patt" then
			pix(g_mx-1,g_my-1,cs)
		end
	end

	local function render_ui()
	bma("ui_render",function()--@bm
		if info_vis then
			GenInfo()
		end
		if opts[USE_TLTIPS] and tb_vis then
			ToolTip()
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
		local pgsc,pgs,tc,hc,sc,fw=printgsc,printgs,thm.text,thm.header,thm.shad,true
		pgsc("Help "..tonum(state:sub(5,5)).."/"..NUM_HELP_SCRS,_,0,hc)
		pgsc("(Any key) >>",_,16,hc,false)
		if state=="help1" then
			pgsc("Screen",1,1,hc)
			pgs(help_strs[1]:gsub('\t',''),0,2,tc,sc,fw)
			spr(256,2*8,5*8+5,0,1,0,0,8,5)
		elseif state=="help2" then
			pgsc("Keyboard",1,1,hc)
			pgs(help_strs[2]:gsub('\t',''),2,2,tc,sc,fw)
			pgsc("Mouse",1,13,hc)
			pgs(help_strs[3]:gsub('\t',''),2,14,tc,sc,fw)
		elseif state=="help3" then
			pgsc("Tools",1,1,hc)
			pgs(help_strs[4]:gsub('\t',''),2,2,tc,sc,fw)
			pgsc("Tool modes",1,10,hc)
			pgs(help_strs[5]:gsub('\t',''),2,11,tc,sc,fw)
		elseif state=="help4" then
			pgsc("Pattern Categories",1,2,hc)
			pgs(help_strs[6]:gsub('\t',''),2,3,tc,sc,fw)
		end
	end

	local function draw_options()
		ui.with_visible(true,function()
			cls(1)
			local tc,hc,lbt,ofg,obg,fg,bg,c,tx=thm.text,thm.header,{shadow=1},cell_col,bg_col
			printgsc("Options ",_,1,hc)

			-- hacky, but avoids wasting tokens on a proper text button for a single use
			if ui.is_under_mouse(111,127,19,7)then
				printgsc("O >>",_,16,6)
				if mbtnr(M1)then toggle_options()end
			else
				printgsc("O >>",_,16,hc)
			end

			tx=24
			for i=1,#opts-1 do
				Switch("s"..i,16,tx+8*(i-1),opts[i],function(t)
					if t.switched then toggle_opt(i)end
				end)
			end
			Switch("s9",16,tx+32,opts[USE_TLTIPS],function(t)
				if t.switched then toggle_opt(USE_TLTIPS)end
			end)
			Label("l1",34,tx+1  ,"Use cell padding",tc,lbt)
			Label("l2",34,tx+1+8,"Wrap around edges",tc,lbt)
			Label("l3",34,tx+1+16,"Randomize at startup",tc,lbt)
			Label("l4",34,tx+1+24,"Reset on randomize",tc,lbt)
			Label("l5",34,tx+1+32,"Enable tooltips",tc,lbt)

			Spinbox("sp1",16,tx+48,opts[ZOOM_LVL],1,4,1,function(t)
				if t.val then set_zoom(t.val,true)end
			end)
			Label("l4",40,tx+48,"Zoom level",tc,lbt)

			fg=ColorPicker("cp1",8,88,"Cell color",1,ofg)
			if fg[1]~=ofg[1]or fg[2]~=ofg[2]or fg[3]~=ofg[3]then
				set_cell_color(fg)
			end
			bg=ColorPicker("cp2",120,88,"Background",2,obg)
			if bg[1]~=obg[1]or bg[2]~=obg[2]or bg[3]~=obg[3]then
				set_bg_color(bg)
			end
		end)
	end

	local function render()
	bma("render",function()--@bm
		cls(thm.bg)
		-- cls(2)
		if     state=="game"then draw_game()
		elseif state~="options"then draw_help()
		else draw_options()
		end
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--



--=--=--=--=--=--=--=--=--=--=--=--=--
-- input
	local function handle_keys()
		if keyp(dbg.key)then dbg:toggle()
		else
			local k,tp=keys,tl.type
			if state=="options"then
				if keyp(k.O)then toggle_options()end
			elseif state~="game"then
				if keyp()or mbtnp()then toggle_help()end
			else
				ctrl=key(k.CTRL)
				shift=key(k.SHFT)
				alt=key(k.ALT)

				tl.set_mods(ctrl,shift,alt)

				if keyp(k.SPACE)then if shift then pause(true)else toggle_pause()end
				elseif keyp(k.G,10,5)then if paused then compute_gen()end
				elseif keyp(k.ENTER)then rand_cells(opts[RAND_RESET])
				elseif keyp(k.PGUP)then inc_zoom()
				elseif keyp(k.PGDN)then dec_zoom()

				elseif shift and key(k.UP)then inc_speed()
				elseif shift and key(k.DOWN)then dec_speed()
				elseif keyp(k.UP)then inc_speed()
				elseif keyp(k.DOWN)then dec_speed()
				elseif keyp(k.O)then toggle_options()
				elseif keyp(k.H)then toggle_help()
				elseif keyp(k.P)then toggle_padding()
				elseif keyp(k.T)then toggle_toolbars()
				elseif keyp(k.TAB)then toggle_ui()
				elseif keyp(k.I)then toggle_info()
				elseif keyp(k.Q)then tl:rot(-1)
				elseif keyp(k.E)then tl:rot(1)
				elseif keyp(k.A)then tl:flip("h")
				elseif keyp(k.Z)then tl:flip("v")
				elseif keyp(k.C)then
					if shift then fill_grid(false)
					else tl:switch(ctrl and"copy"or"circ")
					end
				elseif keyp(k.X)and ctrl then tl:switch("cut")
				elseif keyp(k.V)and ctrl and #tl.clipboard>0 then tl:switch("paste")
				elseif keyp(k.D)or keyp(k.B)or keyp(k.N1)then
					if tp~="brush"then tl:switch("brush")
					else tl:toggle_brush()
					end
				elseif keyp(k.L)then
					if ctrl then load_board()
					else tl:switch("line")
					end
				elseif keyp(k.R)then tl:switch("rect")
				elseif keyp(k.F)then
					if shift then fill_grid(true)
					else tl:switch("fill")
					end
				elseif keyp(k.W,10,5)then
					if tp=="brush"then tl:expand()
					elseif tp=="patt"then tl:next_pattern()
					end
				elseif keyp(k.S,10,5)then
					if ctrl then save_board()
					elseif tp=="brush"then tl:contract()
					elseif tp=="patt"then tl:prev_pattern()
					end
				else
					for i=k.N2,k.N6 do
						if keyp(i)then
							tl:switch("patt")
							tl:set_category(i-k.N2+1)
						end
					end
				end
			end
		end
	end

	local function handle_mouse()
		g_mx,g_my=mx//CS+1,my//CS+1
		g_lmx,g_lmy=lmx//CS+1,lmy//CS+1

		local tp,tm=tl.type,tl.mode
		if state=="game"and not ui.mouse_on_ui then
			if mbtnp(M1)then
				if tm=="erase"then tl:stop()
				else tl:start()
				end
			elseif mbtnp(M2)then
				if tm=="draw"then tl:stop()
				else tl:start(true) -- true for erasing
				end
			elseif mbtnr(M1)and(tp=="copy"or tp=="cut")then
				tl:copy()
			elseif(mbtnr(M1)or mbtnr(M2))and tm then
				if tp=="brush"then tl:stop()
				elseif tp=="fill"then
					if mbtnr(M1)and tm~="erase"or mbtnr(M2)and tm=="erase"then
						tl:commit(true)
					end
				else tl:commit(true) -- true for canceling
				end
			end
			if tp=="brush"and tm then tl:commit()end
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

	function toggle_toolbars()
		tb_vis=not tb_vis
	end

	local _hs_={game="help1",help1="help2",help2="help3",help3="help4",help4="game"}
	function toggle_help()
		state=_hs_[state]
	end

	function toggle_options()
		state=state=="game"and"options"or"game"
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

	function reset()
		gens = 0
		-- fill_grid(false)
		load_board()
		stopped=true
	end

--=--=--=--=--=--=--=--=--=--=--=--=--


function TIC()
bma("Total",function()--@bm
	tm_check()
	update_mst()

	monitor("dt", fmt("%.3f",dt), 11)
	monitor("fps", fmt("%d",1//dt), 11)

	ui.start_frame()
	update_ui()
	input()
	update()
	render()

	ui.end_frame()
	dbg:draw()
end)--@bm
end


init()
