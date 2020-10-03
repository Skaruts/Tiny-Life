
-- title: Tiny Life
-- author: MetalDudeBro
-- desc: A toy Game of Life
-- script: lua
-- input: mouse, keyboard
-- saveid: TinyLife
-- version 1.2
--=--=--=--=--=--=--=--=--=--=--=--=--
-- MIT License (c) 2019 Skaruts (MetalDudeBro)
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- utils & shortenings
	local poke4,print,pix,spr,clip,rect,rectb=poke4,print,pix,spr,clip,rect,rectb

	local t_ins,t_sort,t_rem,t_conct,t_unpk=table.insert,table.sort,table.remove,table.concat,table.unpack
	local floor,ceil,max,min,abs,rand=math.floor,math.ceil,math.max,math.min,math.abs,math.random
	local assert,tonum,tostr,type,setmt,getmt,pairs,ipairs=assert,tonumber,tostring,type,setmetatable,getmetatable,pairs,ipairs
	local gsub,fmt,rep=string.gsub,string.format,string.rep

	local function clamp(v,lo,hi)return max(lo,min(hi,v))end
	local function wrap(v,lo,hi)return v>hi and lo or v<lo and hi or v end
	local function round(a)local floored=floor(a)return a-floored>=0.5 and floored+1or floored end
	local function lerp(a,b,t)return a*(1-t)+b*t end
	local function stonum(v,base)return tonum(v,base)end
	local function btonum(b)return b and 1 or 0 end


	--[[ Print w/ drop shadow           (0.06) ]]
		local shad_c=3 -- shadow_color
		local function prints(t,x,y,c,fix_w,ofx,ofy)
			if not y then return print(t,-999,-999, 0, x) end
			local xc,yc,ofx,ofy=1,1,ofx or 0,ofy or 0
			print(t,x*8+xc+ofx+1,y*8+yc+ofy+1,shad_c,fix_w,1)
			print(t,x*8+xc+ofx,y*8+yc+ofy,c,fix_w,1)
		end
	--[[ Print w/ drop shadow centered  (0.07) ]] local function printc(t,x,y,c,fix_w)local xc,yc,c,len=1,1,c or 15,print(t,-999,-999)x=x==nil and(240-len)//2or x*8 y=y==nil and 136//2-8//2or y*8 print(t,x+xc+1,y+yc+1,shad_c,fix_w)print(t,x+xc,y+yc,c,fix_w)end
	--[[ Trace formatted                (0.01) ]] local function tracef(...)trace(fmt(...))end
	--[[ Trace csv arguments            (0.01) ]] local function tracec(...)trace(t_conct({...},",").."\n")end
	--[[ Debugging utility              (0.08) ]]
		local dbg={
			active=false,
			crammed=false,
			col=6,
			fix_w=true,
			h=0,w=0,vals=nil,
			toggle=function(t)t.active=not t.active end,
			cram_text=function(t,enbl)t.crammed=enbl end,
			draw=function(t)
				if t.active then
					if t.crammed then
						local w=prints(t.vals,t.fix_w)
						rect(0,0,w+8,t.h*8,1)
						prints(t.vals,0,0,t.col,t.fix_w)
						t.vals=""
					else
						local w=t.w*8-t.w*2
						rect(0,0,w+8,t.h*8+8,1)
						for i=1,#t.vals do
							prints(t.vals[i],0,i-1,t.col,t.fix_w)
						end
						t.vals,t.w={},0
					end
					t.h=0
				end
			end,
		}
		dbg.vals=dbg.crammed and""or{}
		local function monitor(k,v)
			local t=dbg
			if t.active then
				if t.crammed then
					if v==nil then t.vals=t_conct({t.vals,k,'\n'})
					elseif k~=""then t.vals=t_conct({t.vals,k,tostr(v),'\n'})
					else t.vals=t_conct({t.vals,tostr(v),'\n'})
					end
				else
					local s
					if v==nil then s=k
					elseif k~=""then s=t_conct({k,tostr(v)})
					else s=tostr(v)
					end
					t.vals[#t.vals+1]=s
					if #s>t.w then t.w=#s end
				end
				t.h=t.h+1
			end
		end
		local function bm(name,f)  -- benchmark
			local t1=time()
			f()
			monitor(name,fmt("%.2f",time()-t1).."ms")
		end
		local function bma(name,f) -- benchmark aligned
			local t1=time()
			f()
			monitor(name..rep(' ',14-#name),fmt("%.2f",time()-t1).."ms")
		end
	--[[ Vector2 (stripped)             (0.02) ]]
		local _VECMT={}
		local function _vec_xy(x,y)return setmt({x=x,y=y},_VECMT)end
		_VECMT={
			__index=_VECMT,
			__tostring=function(t)return fmt("(%s,%s)",t.x,t.y)end,
			__add=function(a,b)return type(b)=="number"and _vec_xy(a.x+b,a.y+b)or _vec_xy(a.x+b.x,a.y+b.y)end,
			__sub=function(a,b)return type(b)=="number"and _vec_xy(a.x-b,a.y-b)or _vec_xy(a.x-b.x,a.y-b.y)end,
			__mul=function(a,b)return type(b)=="number"and _vec_xy(a.x*b,a.y*b)or _vec_xy(a.x*b.x,a.y*b.y)end,
			__div=function(a,b)return type(b)=="number"and _vec_xy(a.x/b,a.y/b)or _vec_xy(a.x/b.x,a.y/b.y)end,
			__idiv=function(a,b)return type(b)=="number"and _vec_xy(a.x//b,a.y//b)or _vec_xy(a.x//b.x,a.y//b.y)end,
			__eq=function(a,b)return a.x==b.x and a.y==b.y end,
		}
		local function vec(x,y)
			if not x then return setmt({x=0,y=0},_VECMT)end
			if not y then
				y=x.y or x[2]
				x=x.x or x[1]
			end
			return setmt({x=x,y=y},_VECMT)
		end
	--[[ Rect (stripped)                (0.01) ]]
		local bi_err,_RECREADONLY,_RECI,_RECNI="bad index '%s' for rect",{c=true,tl=true,tr=true,bl=true,br=true},
		{
			x2=function(t,k)return t.x+t.w end,
			y2=function(t,k)return t.y+t.h end,
			c=function(t,k)return vec((t.x+t.x+t.w)//2,(t.y+t.y+t.h)//2)end,
			tl=function(t,k)return vec(t.x,t.y)end,
			tr=function(t,k)return vec(t.x+t.w,t.y)end,
			bl=function(t,k)return vec(t.x,t.y+t.h)end,
			br=function(t,k)return vec(t.x+t.w,t.y+t.h)end,
		},{
			x2=function(t,v)t.w=v-t.x end,
			y2=function(t,v)t.h=v-t.y end,
		}
		local _RECMT={}
		local function _rect(x,y,w,h)return setmt({x=x,y=y,w=w,h=h},_RECMT)end
		local function Rect(x,y,w,h)
			if not x then return setmt({x=0,y=0,w=0,h=0},_RECMT)end
			if not h then
				if not y then return _rect(x.x or x[1],x.y or x[2],x.w or x[3],x.h or x[4])end
				return _rect(x.x or x[1],x.y or x[2],y.x or y[1],y.y or y[2])
			end
			return _rect(x,y,w,h)
		end
		_RECMT={
			__index=function(t,k)
				if _RECMT[k]~= nil then return _RECMT[k]end
				if _RECI[k]then return _RECI[k](t,k)end
				error(fmt(bi_err,tostr(k)))
			end,
			__newindex=function(t,k,v)
				if _RECREADONLY[k]then error(fmt("'%s' is read only",tostr(k)))end
				if _RECNI[k]then return _RECNI[k](t,v)end
				error(fmt(bi_err,tostr(k)))
			end,
			__eq=function(t,o)return t.x==o.x and t.y==o.y and t.w==o.w and t.h==o.h end,
			sq=function(t)
				return _rect(
					t.x, t.y,
					t.w > t.h and t.h or t.w,
					t.w > t.h and t.h or t.w
				)
			end,
		}
	--[[ Bresenham stuff                (0.01) ]]
		local Bres = {
			line=function(x1,y1,x2,y2)
				local pts,dtx,dty={},x2-x1,y2-y1
				local ix,iy=dtx>0 and 1or-1,dty>0 and 1or-1
				dtx,dty=2*abs(dtx),2*abs(dty)
				pts[#pts+1]=_vec_xy(x1,y1)
				if dtx>=dty then
					err=dty-dtx/2
					while x1~=x2 do
						if err>0or(err==0 and ix>0)then
							err,y1=err-dtx,y1+iy
						end
						err,x1=err+dty,x1+ix
						pts[#pts+1]=_vec_xy(x1,y1)
					end
				else
					err=dtx-dty/2
					while y1~=y2 do
						if err>0or(err==0 and iy>0)then
							err,x1=err-dty,x1+ix
						end
						err,y1=err+dtx,y1+iy
						pts[#pts+1]=_vec_xy(x1,y1)
					end
				end
				return pts
			end,
		}
	--[[ mouse states                   (0.03) ]]
		--TODO: could I use binary for the button states
		local
		mx,my,lmb,mmb,rmb,mwx,mwy,
		M1,M2,M3,LMB,RMB,MMB,m_stt,
		mbtn,mbtnp,mbtnr,mbtnt,update_mouse=
		0,0,false,false,false,0,0,
		1,2,3,1,2,3,{
			prev={0,0,0}, -- left, right, middle
			curr={0,0,0}  -- left, right, middle
		}
		mbtn=function(b)
			if b then return m_stt.curr[b]>0 end
			return m_stt.curr[1]>0 or m_stt.curr[2]>0 or m_stt.curr[3]>0
		end
		mbtnp=function(b)
			if b then return m_stt.curr[b]==1 end
			return m_stt.curr[1]==1 or m_stt.curr[2]==1 or m_stt.curr[3]==1
		end
		mbtnr=function(b)return m_stt.prev[b]>0 and m_stt.curr[b]==0 end
		mbtnt=function(b)return m_stt.curr[b]end
		update_mouse=function()
			mx,my,lmb,mmb,rmb,mwx,mwy=mouse()
			m_stt.prev={m_stt.curr[1],m_stt.curr[2],m_stt.curr[3]}
			m_stt.curr={0,0,0}
			if lmb then m_stt.curr[1]=m_stt.prev[1]+1 end
			if rmb then m_stt.curr[2]=m_stt.prev[2]+1 end
			if mmb then m_stt.curr[3]=m_stt.prev[3]+1 end
		end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Ticuare
	-- TICuare - A UI library for TIC-80
	--
	-- Copyright (c) 2017 Crutiatix
	-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
	-- The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
	-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	tc={name="tc",elements={},z=1,hz=nil}tc.__index=tc tc.me={nothing=0,click=1,noclick=2,none=3}local e={__index=tc}local function u(r,e,n,t,o,i)return r>n and r<n+o and e>t and e<t+i end local function R(e,o,t)for n,r in pairs(o)do if type(r)=="table"then if type(e[n]or false)=="table"then R(e[n]or{},o[n]or{},t)else if not e[n]or t then e[n]=r end end else if not e[n]or t then e[n]=r end end end return e end local function i(e,n)if(e==nil and	n==nil)then for e=0,15 do poke4(16368*2+e,e)end else poke4(16368*2+e,n)end end function tc.lerp(e,n,t)if e==n then return e else if abs(e-n)<.005 then return n else return e*(1-t)+n*t end end end local function F(o)local t={}local function r(e)if type(e)~="table"then return e elseif t[e]then return t[e]end local n={}t[e]=n for t,e in pairs(e)do n[r(t)]=r(e)end return setmt(n,getmt(e))end return r(o)end local function f(o,r,t,n,e)if o then return t elseif r then return n else return e end end function tc.print(t,i,a,o,n,e)n=n or false e=e or 1 local d,r=gsub(t,"\n","")local w,h if o then w,h=print(t,i,a,o,n,e),(6+r)*e*(r+1)end return w,h end function tc.font(c,s,f,r,e,a,n,o,t)e=e or-1 a=a or 8 n=n or 8 o=o or false t=t or 1 local
	d,l=gsub(c,"\n","")if type(e)=="table"and type(e[1])=="table"then for t,n in ipairs(e[1])do if type(r)=="table"then i(n,r[t])else i(n,r)end end e=e[2]end local d if r then d=font(c,s,f,e,a,n,o,t)end i()return d,(n+l)*t*(1+l)end function tc.element(t,n)if not n then n=t t="element"end local e=n setmt(e,tc)e.hover,e.click=false,false e.activity=n.activity or true e.drag=n.drag or{activity=false}e.align=n.align or{x=0,y=0}e.visibility=n.visibility or true if e.content then if not e.content.scroll then e.content.scroll={x=0,y=0}end e.content.w,e.content.h=e.content.w or e.w,e.content.h or e.h end e.type,e.z=t,tc.z tc.z=tc.z+1 tc.hz=tc.z t_ins(tc.elements,e)return e end function tc.Element(e)return tc.element("element",e)end function tc.Style(e)return e end function tc.Group()local e={type="group",elements={}}setmt(e,tc)return e end function tc:updateSelf(e)if e.mouse_x and e.mouse_y and e.event then mouse_x=e.mouse_x mouse_y=e.mouse_y mouse_press=e.press mouse_event=e.event local a,d,n,t,r,e,e local e,i,o=tc.me,self.x-(self.align.x==1 and self.w*.5 or(self.align.x==2 and self.w or 0)),self.y-(self.align.y==1 and self.h*.5-1 or(self.align.y==2
	and self.h-1 or 0))a=mouse_event~=e.none and mouse_press or false d=u(mouse_x,mouse_y,i,o,self.w,self.h)n=mouse_event~=e.none and d or false t,r=self.hover,self.hold self.hover=n or(self.drag.active and tc.draging_obj and tc.draging_obj.obj==self)self.hold=((mouse_event==e.click and n)and true)or(a and self.hold)or((n and mouse_event~=e.noclick and self.hold))if mouse_event==e.click and n and self.onClick then self.onClick(self)elseif(mouse_event==e.noclick and n and r)and self.onCleanRelease then self.onCleanRelease(self)elseif((mouse_event==e.noclick and n and r)or(self.hold and	not n))and self.onRelease then self.onRelease(self)elseif self.hold and self.onPress then self.onPress(self)elseif not t and self.hover and	self.onStartHover then self.onStartHover(self)elseif self.hover and self.onHover then self.onHover(self)elseif t and not self.hover and self.onEndHover then self.onEndHover(self)end if self.hold and(not n or self.drag.active)and not tc.draging_obj then self.hold=self.drag.active	tc.draging_obj={obj=self,d={x=i-mouse_x,y=o-mouse_y}}elseif not self.hold and n and(tc.draging_obj and tc.draging_obj.obj==self)then self.hold=true
	tc.draging_obj=nil end if tc.draging_obj and tc.draging_obj.obj==self and self.drag.active then self.x=(not self.drag.fixed or not self.drag.fixed.x)and mouse_x+tc.draging_obj.d.x or self.x self.y=(not self.drag.fixed or not self.drag.fixed.y)and mouse_y+tc.draging_obj.d.y or self.y local e=self.drag.bounds if e then if e.x then self.x=(e.x[1]and self.x<e.x[1])and e.x[1]or self.x self.x=(e.x[2]and self.x>e.x[2])and e.x[2]or self.x end if e.y then self.y=(e.y[1]and self.y<e.y[1])and e.y[1]or self.y self.y=(e.y[2]and self.y>e.y[2])and e.y[2]or self.y end end if self.track then self:anchor(self.track.ref)end end return n elseif e.focused_element and e.event then local t,i,a,n,r,o=tc.me i=e.event~=t.none and e.press or false a=self==e.focused_element n=e.event~=t.none and a or false r,o=self.hover,self.hold self.hover=n self.hold=((e.event==t.click and n)and true)or(i and self.hold)or((n and e.event~=t.noclick and self.hold))if e.event==t.click and n and self.onClick then self.onClick(self)elseif(e.event==t.noclick and n and o)and self.onCleanRelease then self.onCleanRelease(self)elseif((e.event==t.noclick and n and o)or(self.hold and not n))and self.onRelease then
	self.onRelease(self)elseif self.hold and self.onPress then self.onPress(self)elseif not r and self.hover and self.onStartHover then self.onStartHover(self)elseif self.hover and self.onHover then self.onHover(self)elseif r and not self.hover and self.onEndHover then self.onEndHover(self)end return n	else error("updateSelf error in arguments!")end end function tc:updateTrack()local n,e=self.drag.bounds,self.track if e then self.x,self.y=e.ref.x+e.d.x,e.ref.y+e.d.y if n and n.relative then if n.x then n.x[1]=e.ref.x+e.b.x[1]or nil n.x[2]=e.ref.x+e.b.x[2]or nil end if n.y then n.y[1]=e.ref.y+e.b.y[1]or nil n.y[2]=e.ref.y+e.b.y[2]or nil end end end end function tc:drawSelf()if self.visibility then local S,C,R,h,j,v,u,P,o,r,m,_,p,b,k,z,s,x,e,i,l,d,H,y,g local c,a,e,t,n,w=self.shadow,self.border,self.text,self.icon,self.tiled,self.colors o=self.x-(self.align.x==1 and self.w*.5-1 or(self.align.x==2 and self.w-1 or 0))r=self.y-(self.align.y==1 and self.h*.5-1 or(self.align.y==2 and self.h-1 or 0))if c and c.colors then c.offset=c.offset or{x=1,y=1}C=f(self.hold,self.hover,c.colors[3],c.colors[2],c.colors[1])if C then rect(o+c.offset.x,r+c.offset.y,self.w,self.h,C)end end if
	w then S=f(self.hold,self.hover,w[3],w[2],w[1])if S then rect(o,r,self.w,self.h,S)end end i=a and(a.width)or 0 y=2*i if n then n.scale=n.scale or 1 n.key=n.key or-1 n.flip=n.flip or 0 n.rotate=n.rotate or 0 n.w=n.w or 1 n.h=n.h or 1 H=f(self.hold,self.hover,n.sprites[3],n.sprites[2],n.sprites[1])if H then clip(o+i,r+i,self.w-y,self.h-y)for e=0,self.w+(8*n.w)*n.scale,(8*n.w)*n.scale do for t=0,self.h+(8*n.h)*n.scale,(8*n.h)*n.scale do spr(H,o+e+i,r+t+i,n.key,n.scale,n.flip,n.rotate,n.w,n.h)end end clip()end end if self.content and self.drawContent then if self.content.wrap and clip then clip(o+i,r+i,self.w-y,self.h-y)end self:renderContent()if self.content.wrap and clip then clip()end end if a and a.colors then k=a.colors R=f(self.hold,self.hover,k[3],k[2],k[1])if R then for e=0,a.width-1 do rectb(o+e,r+e,self.w-2*e,self.h-2*e,R)end end end if a and a.sprites then l=a.key or-1 d=f(self.hold,self.hover,a.sprites[3],a.sprites[2],a.sprites[1])if d then clip(o+8,r,self.w-16+1,self.h)for e=8,self.w-9,8 do spr(d[2],o+e,r,l,1,0,0)spr(d[2],o+e,r+self.h-8,l,1,0,2)end clip()spr(d[1],o,r,l,1,0,0)spr(d[1],o+self.w-8,r,l,1,0,1)clip(o,r+8,self.w,self.h-16+1)for e=8,self.h-9,8 do spr(d[2],
	o,r+e,l,1,0,3)spr(d[2],o+self.w-8,r+e,l,1,2,1)end clip()spr(d[1],o+self.w-8,r+self.h-8,l,1,0,2)spr(d[1],o,r+self.h-8,l,1,0,3)end end if t and t.sprites and#t.sprites>0 then P=((self.hold and t.sprites[3])and t.sprites[3])or((self.hover and t.sprites[2])and t.sprites[2])or t.sprites[1]z=t.offset or{x=0,y=0}t.align=t.align or{x=0,y=0}spr(P,(o+(t.align.x==1 and self.w*.5-((t.scale*8)/2)or(t.align.x==2 and self.w-(t.scale*8)or 0))+z.x),(r+(t.align.y==1 and self.h*.5-((t.scale*8)/2)or(t.align.y==2 and self.h-(t.scale*8)or 0))+z.y),t.key,t.scale,t.flip,t.rotate,t.w,t.h)end if e and e.print then u=e.colors or{15,15,15}u[1]=u[1]or 15 j=f(self.hold,self.hover,u[3],u[2],u[1])if e.shadow then v=e.shadow h=f(self.hold,self.hover,v.colors[3],v.colors[2],v.colors[1])x=v.offset or{x=1,y=1}end s=e.offset or{x=0,y=0}if e.font then e.space=e.space or{w=8,h=8}m,_=tc.font(e.print,0,200,-1,e.key,e.space.w,e.space.h,e.fixed,e.scale)else m,_=tc.print(e.print,0,200,-1,e.fixed,e.scale)end g=e.align or{x=0,y=0}p=(g.x==1 and o+((self.w*.5)-(m*.5))+s.x or(g.x==2 and o+((self.w)-(m))+s.x-i or o+s.x+i))b=(g.y==1 and r+((self.h*.5)-(_*.5))+s.y or(g.y==2 and r+((self.h)-(_))+s.y-i or r+
	s.y+i))if e.font then if type(h)=="table"then tc.font(e.print,p+x.x,b+x.y,h,e.key,e.space.w,e.space.h,e.fixed,e.scale)end tc.font(e.print,p,b,j,e.key,e.space.w,e.space.h,e.fixed,e.scale)else if h then tc.print(e.print,p+x.x,b+x.y,h,e.fixed,e.scale)end tc.print(e.print,p,b,j,e.fixed,e.scale)end end end end function tc:renderContent()local i,o,n,t,r,e e=self.align i=self.x-(e.x==1 and self.w*.5 or(e.x==2 and self.w or 0))o=self.y-(e.y==1 and self.h*.5-1 or(e.y==2 and self.h-1 or 0))n=self.border and self.border.width or 1 t=i-(self.content.scroll.x or 0)*(self.content.w-self.w)+n r=o-(self.content.scroll.y or 0)*(self.content.h-self.h)+n self.drawContent(self,t,r)end function tc:Content(e)self.drawContent=e return self end function tc:scroll(e)if e~=nil then e.x=e.x or 0 e.y=e.y or 0 if self.content then e.x=(e.x<0 and 0)or(e.x>1 and 1)or e.x e.y=(e.y<0 and 0)or(e.y>1 and 1)or e.y self.content.scroll.x,self.content.scroll.y=e.x or self.content.scroll.x,e.y or self.content.scroll.y end return self else if self.content then return self.content.scroll end end end function tc.update(e,l,o)local t,r=tc.me,tc.elements local d,i,a,n=t.nothing,false,{},nil if type(e)=="table"
	then o=l end if e then if tc.click and not o then tc.click=false d=t.noclick tc.draging_obj=nil elseif not tc.click and o then tc.click=true d=t.click tc.draging_obj=nil end for e=1,#r do t_ins(a,r[e])end t_sort(a,function(n,e)return n.z>e.z end)for r=1,#a do n=a[r]if n then if type(e)=="table"then if n:updateSelf{focused_element=e,press=o,event=(i or not n.activity)and t.none or d}then i=true end elseif e and l and type(e)~="table"then if n:updateSelf{mouse_x=e,mouse_y=l,press=o,event=((i or(tc.draging_obj and tc.draging_obj.obj~=n))or not n.activity)and t.none or d}then i=true end else error("Wrong arguments for update()")end end end for e=#r,1,-1 do if r[e]then r[e]:updateTrack()end end end end function tc.draw()local e={}for n=1,#tc.elements do if tc.elements[n].draw then t_ins(e,tc.elements[n])end end t_sort(e,function(n,e)return n.z<e.z end)for n=1,#e do e[n]:drawSelf()end end function tc:style(e)if self.type=="group"then for t,n in pairs(self.elements)do R(n,F(e),false)end else R(self,F(e),false)end return self end function tc:anchor(n)if self.type=="group"then for t,e in pairs(self.elements)do e:anchor(n)end else local e,t,r,o,i=self.drag.bounds,
	nil,nil,nil,nil if e and e.x then t=e.x[1]-n.x r=e.x[2]-n.x elseif e and e.y then o=e.y[1]-n.y i=e.y[2]-n.y end self.track={ref=n,d={x=self.x-n.x,y=self.y-n.y},b={x={t,r},y={o,i}}}end return self end function tc:group(n,e)if e then n.elements[e]=self else t_ins(n.elements,self)end return self end function tc:active(e)if e~=nil then if self.type=="group"then for t,n in pairs(self.elements)do n:active(e)end else self.activity=e end return self else if self.type=="group"then local e={}for n,t in pairs(self.elements)do e[n]=t:active()end return e else if self.activity~=nil then return self.activity end end end end function tc:visible(e)if e~=nil then if self.type=="group"then for t,n in pairs(self.elements)do n:visible(e)end else self.visibility=e end return self else if self.type=="group"then local e={}for n,t in pairs(self.elements)do e[n]=t:visible()end return e else if self.activity~=nil then return self.visibility end end end end function tc:dragBounds(e)if e~=nil then self.drag.bounds=e else return self.drag.bounds end end function tc:horizontalRange(n)local e=self.drag.bounds if n~=nil then self.x=e.x[1]+(e.x[2]-e.x[1])*n else assert(e and e.x and#e.x==2,
	"X bounds error!")return(self.x-e.x[1])/(e.x[2]-e.x[1])end end function tc:verticalRange(n)local e=self.drag.bounds if n~=nil then self.y=e.y[1]+(e.y[2]-e.y[1])*n else assert(e and e.y and#e.y==2,"Y bounds error!")return(self.y-e.y[1])/(e.y[2]-e.y[1])end end function tc:index(e)if e~=nil then if self.type=="group"then local n for t,e in pairs(self.elements)do if not n or e.z<n then n=e.z end end for r,t in pairs(self.elements)do local e=t.z-n+e t:index(e)end else self.z=e if e>tc.hz then tc.hz=e end end else return self.z end return end function tc:toFront()if self.z<tc.hz or self.type=="group"then return self:index(tc.hz+1)end end function tc:remove()for e=#tc.elements,1,-1 do if tc.elements[e]==self then t_rem(tc.elements,e)self=nil end end end function tc.empty()for e=1,#tc.elements do tc.elements[e]=nil end end

	-- (Skaruts) convenience functions (also helps keeping under 64k)
	function tc.enable(t)
		t:visible(true)
		t:active(true)
	end
	function tc.disable(t)
		t:visible(false)
		t:active(false)
	end
	function tc.set_enabled(t, bool)
		t:visible(bool)
		t:active(bool)
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- setup
	-- in-game options/load values or defaults
	local draw_d_cells, wrap_around, use_padding, rand_start, rand_reset =1,2,3,4,5
	local opts={false, true, true, true, false}
	if pmem(draw_d_cells) ~= 0 then opts[draw_d_cells] = true end
	if pmem(wrap_around) ~= 1 then opts[wrap_around] = false end
	if pmem(use_padding) ~= 1 then opts[use_padding] = false end
	if pmem(rand_start) ~= 1 then opts[rand_start] = false end
	if pmem(rand_reset) ~= 0 then opts[rand_reset] = true end

	function toggle_opt(i) -- toggle value of an option and save it
		local b=not opts[i]
		pmem(i,b and 1or 0)
		opts[i]=b
	end

	local dbg_times={ -- debug
		input="0ms",
		update="0ms",
		ui_update="0ms",
		gen="0ms",
		render="0ms",
		ui_render="0ms",
		render_dbg="0ms",
	}
	local keys={
		PGUP=54,PGDN=55,SPACE=48,ENTER=50,BSLASH=41,GRAVE=44,TAB=49,
		A=1,B=2,C=3,D=4,E=5,F=6,G=7,H=8,I=9,J=10,K=11,L=12,M=13,N=14,O=15,P=16,
		Q=17,R=18,S=19,T=20,U=21,V=22,W=23,X=24,Y=25,Z=26,
		N0=27,N1=28,N2=29,N3=30,N4=31,N5=32,N6=33,N7=34,N8=35,N9=36,
		CTRL=63,SHFT=64,ALT=65,
	}
	-- zoom scalar (higher = smaller cells, less performance)
	-- use 1, 2, 4 or 8
	-- (NOTE: drawing tools get a bit buggy with
	-- low fps unless you pause before drawing)
	local zoom_lvl=8
	local g_mx,g_my=0,0 -- grid mouse pos
	local ct_mod,sh_mod,al_mod,mouse_on_ui=false,false,false,false
	local cols={
		bg=0,
		text=5,
		header=9,
		dead=2,
		alive=8,
		selected=11,
		erasing=7,
		ui_lines=13,
		dim_text=14,
		paused=7,
	}
	local pre,cur,sel=1,2,3 -- prev/curr/ghost buffer indices
	local cells={} -- cell buffers
	local SS,NSW,NSH=8,30,17 -- sprite size, num sprites wide / high
	local CS,GW,GH=SS//zoom_lvl,NSW*zoom_lvl,NSH*zoom_lvl -- cell size, grid width/height
	local pad=0 -- padding for cell rects (always 0 if 'zoom_lvl < 8' -- see 'set_padding()')
	local anim,ui_vis=nil,true
	local paused,stopped,eraser,t_old=true,true,false,0
	local bar_outline,bar_bg=132,255 -- tiles
	local use_topbar,use_ibar=false,false -- starting state is inverted for these
	local l_cells,gens,TOT_CELLS=0,0,0 -- living cells / generations
	local GAME_SCR,HELP_SCR1,HELP_SCR2,HELP_SCR3,HELP_SCR4,OPTS_SCR=1,2,3,4,5,10 -- game screens
	local num_help_scrs,cur_scr=4,GAME_SCR

	-- help strings
	--------------------------
	local help_strs={
		hstr1=[[

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
		hstr2=[[
					H         Cycle help screens
					O         Toggle Options
					SPACE     Pause/play
					SHIFT-SPC Reset (stop)
					SHIFT-C/F Revive/kill all cells
					ENTER     Randomize cells
					K         Draw dead cells
					G         Next Generation (if paused)
					I/T       Toggle UI bars
					TAB/U     Toggle/Reset UI
					PG-UP/DN  Zoom in/out (resets cells)
					P         Toggle padding (if zoom<4)
				]],
		hstr3=[[
					LMB       Draw
					RMB       Erase(brush tool)/cancel
					MMB       Erase(non-brush tools)
				]],
		hstr4=[[
					V/1       Brush tool
					2-6       Pattern tool/categories
					L         Line tool
					R/C       Rectangle/circle tools
					F         Fill tool
				]],
		hstr5=[[
					E         Toggle eraser mode
					CTRL      Filled rect/circle
					SHIFT     Proportional rect/circle
					ALT       Centered rect/circle
					W         Expand brush/next pattern
					S         Shrink brush/prev pattern
				]],
		hstr6=[[
					2 - Static
					3 - Blinkers
					4 - Amusing/Explosive
					5 - Gliders
					6 - Glider Gun!









					Lol Note: The Glider Gun doesn't fit
					  on the screen with zoom level 1
				]]
	}
--=--=--=--=--=--=--=--=--=--=--=--=--

-- fwd decls
local tl,ui,rand_cells

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
-- UI helpers
	local _cur_grp,_cur_anch

	function group(g)_cur_grp=g end
	function anchor(a)_cur_anch=a end

	function _chk_grp_ancr(e)
		if _cur_grp then e:group(_cur_grp)end
		if _cur_anch then e:anchor(_cur_anch)end
		return e
	end

	function _icon(i1,i2,i3,kc,sc,w,h)
		return {sprites={i1,i2,i3},key=kc or 0,scale=sc or 1,w=w or 1,h=h or 1}
	end

	function _text(tx,c1,c2,c3,ax,ay,fix,ofx,ofy,s1,s2,s3)
		return {print=tx,colors={c1,c2,c3},align={x=ax or 0,y=ay or 0},
			fixed=fix or false,offset={x=ofx or 0, y=ofy or 0},
			shadow={colors={s1 or 3,s2 or 3,s3 or 3}}}
	end

	function UIGroup()
		return _chk_grp_ancr(tc.Group())
	end

	function UIElem(x,y,w,h)
		return _chk_grp_ancr(tc.element({
			x=x,y=y,w=w,h=h,
			set_pos=function(t,x,y)t.x,t.y=x,y end,
			set_size=function(t,w,h)t.w,t.h=w,h end
		}))
	end

	function Label(x,y,tx,c,ax,ay,fix)
		local e=UIElem(x,y,0,0)
		e.text = _text(tx,c,c,c,ax,ay)
		e.set_text=function(t,tx)t.text.print=tx end
		return e
	end

	function TxtBtn(x,y,tx,fn,args)
		local e=UIElem(SS*x,SS*y,print(tx,0,-99)+2,8)
		e.text=_text(tx,9,6,6,1,1,false,1,1)
		e.onCleanRelease=function(t)if fn then fn(args and t_unpk(args)or nil)end end
		return e
	end

	function BaseBtn(x,y,w,h,i,nfo)
		local e=UIElem(x,y,w,h)
		if i then e.icon=_icon(i,i+1,i+2)end
		if nfo then e.onHover=function(t)set_tooltip(nfo)end end
		return e
	end

	function TlBarHandle(i,x,y,nfo)
		local e=UIElem(x,y,SS*2,SS)
		e.ox,e.oy=x,y
		e.cooldown=0.5
		e.click_t=0
		e.count_t=false
		e.icon=_icon(i,i,i,0,1,2,1)
		e.drag={active=false,bounds={x={0,240-8},y={0,136-8}}}
		e.onHover=function(t)set_tooltip(nfo)end
		e.onPress=function(t)t.drag.active=true end
		e.onCleanRelease=function(t)t.drag.active=false end
		e.onClick=function(t)
			if not t.count_t then
				t:start_t()
			else
				local t1=time()/1000 -- TODO: redundant division?
				local diff=abs(t.click_t-t1)
				if diff<t.cooldown then
					t:reset()
				else
					t:start_t()
				end
			end
		end
		e.start_t=function(t)t.click_t,t.count_t=time()/1000,true end
		e.set_origin=function(t,x,y)
			t.ox,t.oy=x,y
			t:set_pos(x,y)
		end
		e.reset=function(t)
			t:set_pos(t.ox,t.oy)
			t.count_t,t.click_t=false,0
		end
		return e
	end

	function TLModeIcon(x,y,i,nfo)
		local e=BaseBtn(x*SS,y*SS,SS,SS,nil,nfo)
		e.set_state=function(t,bool)
			t.icon=bool and _icon(i+1,i+1,i+1)or _icon(i,i,i)
		end
		return e
	end

	function Sep(i,x,y,w,h)
		local e=UIElem(x,y,0,0)
		e.icon=_icon(i,i+1,i+2,0,1,w,h)
		return e
	end

	function Btn(x,y,i,nfo,fn,args)
		local e=BaseBtn(x,y,SS,SS,i,nfo)
		e.onCleanRelease=function(t)t:toggle_state()end
		e.toggle_state=function(t)t:hit()end
		e.hit=function(t)
			if fn then fn(args and t_unpk(args)or nil)end
		end
		return e
	end

	function ToggleBtn(x,y,i,nfo,par,act_on,fn,args)
		local e=Btn(x,y,i,nfo,fn,args)
		e.icon1=_icon(i,i+1,i+2)
		e.icon2=_icon(i+3,i+4,i+5)
		e.is_on=false
		e.act_on=act_on
		e.parent=par
		e.toggle_state=function(t)
			if t.act_on=="sticky"then
				if not t.is_on then
					t.is_on=true
					if t.parent then t.parent:reset_tls()end
					t.icon=t.icon2
				end
				t:hit()
			else
				t.is_on=not t.is_on
				if t.is_on then
					if t.parent then t.parent:reset_tls()end
					t.icon=t.icon2
					if t.act_on=="on"or t.act_on=="toggle"then t:hit()end
				else
					if t.parent then t.parent:reset_tls()end
					t.icon=t.icon1
					if t.act_on=="off"or t.act_on=="toggle"then t:hit()end
				end
			end
		end
		e.turn_off=function(t)t.icon,t.is_on=t.icon1,false end
		e.turn_on=function(t)t.icon,t.is_on=t.icon2,true end
		return e
	end


	local cb_i = 232
	function CheckBox(x,y,tx,is_on,fn,args)
		Label(x+SS*2,y+1,tx,cols.text)

		local e=Btn(x,y,cb_i,nil,fn,args)
		e.icon1=_icon(cb_i,cb_i+1,cb_i+2)
		e.icon2=_icon(cb_i+3,cb_i+4,cb_i+5)
		e.icon=is_on and e.icon2 or e.icon1
		e.toggle_state=function(t)
			is_on=not is_on
			t:hit()
			t.icon=is_on and t.icon2 or t.icon1
		end
		e.turn_off=function(t)t.icon=t.icon1 end
		e.turn_on=function(t)t.icon=t.icon2 end
		return e
	end

	function UIPanel(bg_i,brdr_i,x,y,w,h,kc)
		local e=UIElem(x,y,w,h)
		e.tiled={sprites={bg_i,bg_i,bg_i},scale=1,key=kc or -1}
		e.border={sprites={{brdr_i,brdr_i+1},{brdr_i,brdr_i+1},{brdr_i,brdr_i+1}},key=0,width=1}
		return e
	end

	function ToolBtn(x,y,i,nm,k,par,fn,args)
		local e=Btn(x*SS,y*SS,i,nm.." Tool "..k,fn,args)
		e.icons={_icon(i,i+1),_icon(i+2,i+3),_icon(i+4,i+5)}
		e.is_on=false
		e.parent=par
		e.onClick,e.onCleanRelease=e.onCleanRelease,nil -- swap
		e.toggle_state=function(t)
			t.is_on=not t.is_on
			if t.is_on then
				t.parent:reset_tls()
				t.icon=t.icons[eraser and 3or 2]
				t:hit()
			else
				t.icon=t.icons[1]
			end
		end
		e.turn_off=function(t)
			t.is_on=false
			t.icon=t.icons[1]
		end
		e.turn_on=function(t)
			t.is_on=true
			t.icon=t.icons[eraser and 3or 2]
		end
		return e
	end

	function Toolbar(i,x,y,w,h)
		local e=UIPanel(i+2,i,x-1,y-1,w+2,h+2)
		e.tls={}
		e.set_pos=function(t,x,y)t.x,t.y=x-1,y-1 end -- override
		e.reset_tls=function(t)for _,b in pairs(t.tls)do b:turn_off()end end
		return e
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- UI init
	ui={
		grp_ui,grp_tlbar,grp_ibar,grp_tbar,grp_rect,grp_circ,grp_play_btn,grp_pause_btn,grp_opts,
		ui_bg,tlbar,ibar,tbar,tlbar_handle,
		lbl_gens,lbl_cells1,lbl_cells2,lbl_cells3,lbl_cells4,lbl_nfo,lbl_help,lbl_mouse,
		btn_rand,btn_decel,btn_accel,btn_stop,btn_play,btn_pause,btn_help,btn_opts,btn_opts_out,
		icn_r_sqr,icn_r_fill,icn_r_cntr,icn_c_sqr,icn_c_fill,icn_c_cntr,
		cbx_pad,
	}

	function init_tlbar()
		group(ui.grp_ui)
			ui.grp_tlbar=UIGroup()

		group(ui.grp_tlbar)
			ui.tlbar_handle=TlBarHandle(128,0,0,"Drag me! (U/2xM1 to reset)")

			ui.grp_play_btn=UIGroup()
			ui.grp_pause_btn=UIGroup()

			anchor(ui.tlbar_handle)
				ui.tlbar=Toolbar(bar_outline,0,0,SS*2,SS*11)

				ui.btn_rand=Btn(0,SS*1,136,"Randomize cells",rand_cells)
				ui.btn_decel=Btn(0,SS*2,200,"Speed -- (N/A yet)",set_speed(-1))
				ui.btn_accel=Btn(SS*1,SS*2,216,"Speed ++ (N/A yet)",set_speed(1))
				ui.btn_stop=ToggleBtn(0,SS*3,152,"Stop/clear",nil,"sticky",pause, {true})
				ui.btn_play=ToggleBtn(SS*1,SS*3,168,"Play/Pause/Unpause",nil,"toggle",unpause)
				ui.btn_pause=ToggleBtn(SS*1,SS*3,184,"Play/Pause/Unpause",nil,"toggle",pause)

				ui.tlbar.tls["brush"]=ToolBtn(0,5,160,"Brush","(V/1)",ui.tlbar,tl.switch,{"brush"})
				ui.tlbar.tls["line"]=ToolBtn(1,5,208,"Line","(L)",ui.tlbar,tl.switch,{"line"})
				ui.tlbar.tls["rect"]=ToolBtn(0,6,176,"Rectangle","(R)",ui.tlbar,tl.switch,{"rect"})
				ui.tlbar.tls["circle"]=ToolBtn(1,6,192,"Circle","(C)",ui.tlbar,tl.switch,{"circle"})
				ui.tlbar.tls["fill"]=ToolBtn(0,7,224,"Fill","(F)",ui.tlbar,tl.switch,{"fill"})
				ui.tlbar.tls["pattern"]=ToolBtn(1,7,240,"Pattern","(2-6)",ui.tlbar,tl.switch,{"pattern"})
				Sep(130,0,SS*4,2,1)
				Sep(130,0,SS*8,2,1)
			anchor()
			ui.grp_rect=UIGroup()
			ui.grp_circ=UIGroup()
		group()

		anchor(ui.tlbar_handle)
			group(ui.grp_rect)
				ui.icn_r_sqr=TLModeIcon(0,9,150,"Proportional (shift)")
				ui.icn_r_fill=TLModeIcon(1,9,166,"Filled (ctrl)" )
				ui.icn_r_cntr=TLModeIcon(0,10,182,"Centered (alt)")

			group(ui.grp_circ)
				ui.icn_c_sqr=TLModeIcon(0,9,198,"Proportional (shift)")
				ui.icn_c_fill=TLModeIcon(1,9,214,"Filled (ctrl)" )
				ui.icn_c_cntr=TLModeIcon(0,10,230,"Centered (alt)")
			group()
		anchor()

		ui.grp_rect:disable()
		ui.grp_circ:disable()

		ui.tlbar_handle.set_origin(ui.tlbar_handle,0,26)
		ui.tlbar_handle:toFront()
		ui.btn_pause:disable()
		anim:add_anim("tlbar_hide",ui.tlbar_handle,ui.tlbar_handle.x-SS*2-1,ui.tlbar_handle.y)
		anim:add_anim("tlbar_show",ui.tlbar_handle,ui.tlbar_handle.x,ui.tlbar_handle.y)
	end

	function init_ibar()
		group(ui.grp_ui)
			ui.grp_ibar = UIGroup()

		group(ui.grp_ibar)
			ui.ibar = UIPanel(bar_bg, bar_outline, 0, 0, SS*30+2, SS*1+2)
			anchor(ui.ibar)
				ui.lbl_nfo=Label(SS*8,2,"",cols.text)
				ui.lbl_mouse=Label(2,2,"",cols.dim_text)
				ui.btn_opts=TxtBtn(26,0,"(O)",toggle_options)
				ui.btn_help=TxtBtn(28,0,"(H)",toggle_help)
			anchor()
		group()

		ui.ibar:set_pos(-1, SS*16-1)

		anim:add_anim("ibar_hide", ui.ibar, ui.ibar.x, ui.ibar.y+SS+1)
		anim:add_anim("ibar_show", ui.ibar, ui.ibar.x, ui.ibar.y)

		toggle_ibar()
	end

	function init_topbar()
		group(ui.grp_ui)
			ui.grp_tbar = UIGroup()
		group(ui.grp_tbar)
			ui.tbar = UIPanel(bar_bg, bar_outline, 0, 0, SS*30+2, SS*1+2)

			anchor(ui.tbar)
				Label(SS*15-1, 2, "C:         |         /", cols.text)
				ui.lbl_gens   = Label(2,  2, "Sp: 1", cols.text)
				ui.lbl_gens   = Label(SS*6,  2, "G: ", cols.text)
				ui.lbl_cells2 = Label(SS*20, 2, "",    cols.text, 2)
				ui.lbl_cells3 = Label(SS*25, 2, "",    cols.text, 2)
				ui.lbl_cells4 = Label(SS*30, 2, "",    cols.text, 2)
			anchor()
		group()

		ui.btn_stop:turn_on()

		ui.grp_pause_btn:disable()

		ui.tbar:set_pos(-1, -1)

		anim:add_anim("topbar_hide", ui.tbar, ui.tbar.x, ui.tbar.y-SS-1)
		anim:add_anim("topbar_show", ui.tbar, ui.tbar.x, ui.tbar.y)
	end

	function init_options()
		group(ui.grp_ui)
			ui.grp_opts = UIGroup()

		group(ui.grp_opts)
			-- toggle_options
			ui.cbx_deadc = CheckBox(SS*2, SS*4,  "draw dead cells",      opts[draw_d_cells], toggle_opt, {draw_d_cells})
			ui.cbx_pad   = CheckBox(SS*2, SS*5,  "use cell padding",     opts[use_padding],  function()toggle_padding()end)
			ui.rand_cb   = CheckBox(SS*2, SS*6, "reset on randomize",   opts[rand_reset],   toggle_opt, {rand_reset})
			ui.wrap_cb   = CheckBox(SS*2, SS*11,  "wrap around edges",    opts[wrap_around],  toggle_opt, {wrap_around})
			ui.rand_cb   = CheckBox(SS*2, SS*12, "randomize at startup", opts[rand_start],   toggle_opt, {rand_start})

			ui.btn_opts_out=TxtBtn(13, 16," O >> ", toggle_options)
		group()

		ui.grp_opts:disable()
	end

	function set_tooltip(tt)
		ui.lbl_nfo:set_text(tt)
	end

	function toggle_eraser()
		eraser = not eraser
		ui.tlbar:reset_tls()
		ui.tlbar.tls[tl.type]:toggle_state()
	end

	function toggle_d_cells()
		opts[draw_d_cells] = not opts[draw_d_cells]
	end

	function toggle_topbar()
		use_topbar = not use_topbar
		ui.grp_tbar:set_enabled(use_topbar)
	end

	function toggle_ibar()
		use_ibar = not use_ibar
		ui.grp_ibar:set_enabled(use_ibar)
	end

	function init_ui()
		ui.grp_ui=UIGroup()
		ui.ui_bg=tc.element({
			x=0,y=0,w=240,h=136,
			onStartHover=function(t)mouse_on_ui=false end,
			onEndHover=function(t)mouse_on_ui=true end
		})

		init_tlbar()
		init_ibar()
		init_topbar()
		init_options()

		toggle_topbar()
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Flood Fill
	local function _has_pix(v,x,y,s,c,elipse)
		if elipse then
			return x<1or y<1or x>GW or y>GH
			or s[y][x]==1
		else
			return x<1or y<1or x>GW or y>GH
			or s[y][x]==1or c[y][x]==v
		end
	end

	--   Scanline FT
	local function scnln_ft(x, y, ellipse,v)
		local pts={}
		t_ins(pts,_vec_xy(x,y)) -- add the initial point

		local s,c,tidx,pt,set_abv,set_blw,sy=cells[sel],cells[cur],#pts
		repeat
			pt = t_rem(pts)
			set_abv,set_blw,sy,x=true,true,s[pt.y],pt.x
			while not _has_pix(v,x,pt.y,s,c,ellipse)do
				sy[x]=1
				if _has_pix(v,x,pt.y-1,s,c,ellipse)~=set_abv then
					set_abv=not set_abv
					if not set_abv then t_ins(pts,_vec_xy(x,pt.y-1))end
				end
				if _has_pix(v,x,pt.y+1,s,c,ellipse)~=set_blw then
					set_blw=not set_blw
					if not set_blw then t_ins(pts,_vec_xy(x,pt.y+1))end
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
					if not set_abv then t_ins(pts,_vec_xy(x,pt.y-1))end
				end
				if _has_pix(v,x,pt.y+1,s,c,ellipse)~=set_blw then
					set_blw=not set_blw
					if not set_blw then t_ins(pts,_vec_xy(x,pt.y+1))end
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


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Geometry stuff
	-- Rect functions
	local function _rect_hollow(r)
		local s,p1,p2=cells[sel],r.tl,r.br
		for j=p1.y,p2.y do
			s[j][r.x]=1
			s[j][r.x2]=1
		end
		for i=p1.x,p2.x do
			s[r.y][i]=1
			s[r.y2][i]=1
		end
	end

	local function _rect_filled(r)
		local s,sj=cells[sel]
		for j=r.y,r.y2 do
			sj=s[j]
			for i=r.x,r.x2 do
				sj[i]=1
			end
		end
	end

	local function _line(x1, y1, x2, y2)
		local s,pts=cells[sel],Bres.line(x1,y1,x2,y2)
		for i=1,#pts do
			local p=pts[i]
			s[p.y][p.x]=1
		end
	end

	-- Ellipse
	----------------------------------
	local function _put_ellipse_pixels(xc, yc, x, y)
		local s = cells[sel]
		s[min(yc+y,GH)][min(xc+x,GW)]=1
		s[min(yc+y,GH)][max(xc-x, 1)]=1
		s[max(yc-y, 1)][min(xc+x,GW)]=1
		s[max(yc-y, 1)][max(xc-x, 1)]=1
	end

	-- not working properly
	local function bres_ellipse(xc, yc, width, height)
		if width <= 0 or height <= 0 then return end

		local a2, b2 = width*width, height*height
		local fa2, fb2 = 4*a2, 4*b2
		local x, y, sigma = 0, 0, 0

		-- /* first half */
		x = 0
		y = height
		sigma = 2*b2 + a2*(1-2*height)
		while b2*x <= a2*y do
			_put_ellipse_pixels(xc, yc, x, y)
			if sigma >= 0 then
				sigma = sigma + fa2*(1-y)
				y = y-1
			end
			sigma = sigma + b2*((4*x)+6)
			x = x+1
		end
		-- /* second half */
		x = width
		y = 0
		sigma = 2*a2 + b2*(1-2*width)
		while a2*y <= b2*x do
			_put_ellipse_pixels(xc, yc, x, y)
			if sigma >= 0 then
				sigma = sigma + fb2 * (1 - x)
				x = x -1
			end
			sigma = sigma + a2 * ((4 * y) + 6)
			y = y+1
		end
	end

	local function _draw_ellipse(r, center, filled)
		local p1, p2 = r.tl, r.br
		local Rx, Ry = (p2.x-p1.x)//2, (p2.y-p1.y)//2

		if not center then bres_ellipse(p1.x+Rx, p1.y+Ry, Rx, Ry)
		else               bres_ellipse(p1.x, p1.y, Rx, Ry)
		end

		if filled then
			if center then flood_fill(p1.x, p1.y, true)
			else flood_fill(r.c.x+p1.x, r.c.y+p1.y, true)
			end
		end
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- Tools
	tl = {
		type="brush",
		orgn=nil,
		w=0,
		h=0,
		is_drawing,
		mode1=false,  -- filled
		mode2=false,  -- square
		mode3=false,  -- centered on mouse
		is_drawing=false,
		brush_size=0,
		bbox=nil,
		cats={},
		cur_pats={},
		patt_pid=0,
		cur_cat=1,
		cur_pat=1,
	}
	function tl.start(x, y)
		tl.orgn = vec(x, y)
		tl.is_drawing = true
	end

	function tl.stop()
		tl.is_drawing = false
	end

	function tl.toggle(x, y)
		if not tl.is_drawing then
			tl.start(x, y)
		else
			tl.stop()
		end
	end

	function tl.cancel()
		if tl.type ~= nil then
			tl.stop()
		end
	end

	function tl.commit(draw)
		local s,c,sj,cj=cells[sel],cells[cur]
		for j=1,GH do
			sj,cj=s[j],c[j]
			for i=1,GW do
				if sj[i]==1then
					cj[i]=draw and 1or 0
				end
			end
		end
	end

	function tl.clear()
		local s,sj=cells[sel]
		for j=1,GH do
			sj=s[j]
			for i=1,GW do
				sj[i]=0
			end
		end
	end

	function tl.switch(t)
		tl.cancel()
		tl.type = t
		local rect_vis,circ_vis=(t=="rect"and ui_vis),(t=="circle"and ui_vis)
		ui.grp_rect:set_enabled(rect_vis)
		ui.grp_circ:set_enabled(circ_vis)
	end

	local function _set_size()
		tl.w = tl.cats[tl.cur_cat][tl.cur_pat].w
		tl.h = tl.cats[tl.cur_cat][tl.cur_pat].h
	end

	function tl.expand()
		tl.brush_size = min(tl.brush_size+1, 5)
	end

	function tl.contract()
		tl.brush_size = max(tl.brush_size-1, 0)
	end

	local function _brush_pts(x,y)
		-- TODO: improve this crappy brush
		-- r=radius
		local r,s,sj=tl.brush_size,cells[sel]
		for j=y-r,y+r do
			if j>0 and j<=GH then
				sj=s[j]
				for i=x-r,x+r do
					if i>0 and i<=GW then
						sj[i]=1
					end
				end
			end
		end
	end

	local function _rect_pts(x, y)
		if tl.is_drawing then
			local p,s -- rect pos,size
			if tl.mode3 then -- if centered
				p=vec(tl.orgn.x,tl.orgn.y)
				s=vec(abs(x-tl.orgn.x),abs(y-tl.orgn.y))*2
				local r=Rect(p-s//2, s)
				r.x=max(1,r.x)
				r.y=max(1,r.y)
				r.x2=min(GW,r.x2)
				r.y2=min(GH,r.y2)
				tl.bbox=r
			else
				p=vec(min(x,tl.orgn.x),min(y,tl.orgn.y))
				s=vec(abs(x-tl.orgn.x),abs(y-tl.orgn.y))
				tl.bbox=Rect(p,s)
			end
			if tl.mode2 then tl.bbox=tl.bbox:sq()end
			if tl.mode1 then
				_rect_filled(tl.bbox)
			else
				_rect_hollow(tl.bbox)
			end
		end
	end

	local function _circle_pts(x, y)
		if tl.is_drawing then
			tl.bbox = Rect(
				vec(min(x, tl.orgn.x), min(y, tl.orgn.y)),
				vec(abs(x-tl.orgn.x), abs(y-tl.orgn.y))
			)
			if tl.mode2 then tl.bbox = tl.bbox:sq() end

			_draw_ellipse( tl.bbox, tl.mode3, tl.mode1 )
		end
	end

	local function _line_pts(x, y)
		if tl.is_drawing then
			_line(tl.orgn.x, tl.orgn.y, x, y)
		end
	end

	local function _fill_pts(x, y)
		flood_fill(x, y)
	end

	local function _patt_pts(x, y)
		local s,p=cells[sel],tl.cats[tl.cur_cat][tl.cur_pat].layout
		for j=1,tl.h do
			local gy=j+(y-1)-tl.h
			if gy>0 and gy<=GH then
				local sj,pj=s[gy],p[j]
				for i=1,tl.w do
					local gx=i+(x-1)-tl.w
					if gx>0 and gx<=GW then
						sj[gx]=pj[i]
					end
				end
			end
		end
	end

	local _draw_fs = {
		brush   = _brush_pts,
		rect    = _rect_pts,
		circle  = _circle_pts,
		line    = _line_pts,
		fill    = _fill_pts,
		pattern = _patt_pts,
	}

	function tl.draw_points(x, y)
		_draw_fs[tl.type](x, y)
	end

	local function _new_pat(id, name, layout)
		return {
			id = id,
			name = name,
			layout = layout,
			w = #layout[1],
			h = #layout
		}
	end

	function tl.set_category(c)
		tl.cur_cat = c
		tl.cur_pat = tl.cur_pats[tl.cur_cat]
		_set_size()
	end

	function tl.next_pattern()
		local old_pat, new_pat = tl.cur_pat, clamp(tl.cur_pats[tl.cur_cat]+1, 1, #tl.cats[tl.cur_cat])
		if new_pat ~= old_pat then
			tl.cur_pat = new_pat
			tl.cur_pats[tl.cur_cat] = new_pat
			_set_size()
		end
	end

	function tl.prev_pattern()
		local old_pat, new_pat = tl.cur_pat, clamp(tl.cur_pats[tl.cur_cat]-1, 1, #tl.cats[tl.cur_cat])
		if new_pat ~= old_pat then
			tl.cur_pat = new_pat
			tl.cur_pats[tl.cur_cat] = new_pat
			_set_size()
		end
	end

	local function _nid()
		tl.patt_pid = tl.patt_pid + 1
		return tl.patt_pid
	end

	function tl.init_pats()
		tl.cats = {
			{
				_new_pat( _nid(), "1-1 Unnamed", {{0,1,0},{1,0,1},{1,0,1},{0,1,0}} ),
				_new_pat( _nid(), "1-2 Loaf", {{0,1,1,0},{1,0,0,1},{0,1,0,1},{0,0,1,0}} )
			},
			{
				_new_pat( _nid(), "2-1 Toad", {{0,1,1,1},{1,1,1,0}} ),
				_new_pat( _nid(), "2-2 Unnamed", {{0,1,0},{0,1,0},{1,0,1},{0,1,0},{0,1,0},{0,1,0},{0,1,0},{1,0,1},{0,1,0},{0,1,0}} ),
				_new_pat( _nid(), "2-3 Unnamed", {{1,1,0,0},{1,0,0,0},{0,0,0,1},{0,0,1,1}} ),
				_new_pat( _nid(), "2-4 Pulsar", {{0,0,1,1,1,0,0,0,1,1,1,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0},{1,0,0,0,0,1,0,1,0,0,0,0,1},{1,0,0,0,0,1,0,1,0,0,0,0,1},{1,0,0,0,0,1,0,1,0,0,0,0,1},{0,0,1,1,1,0,0,0,1,1,1,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,1,1,1,0,0,0,1,1,1,0,0},{1,0,0,0,0,1,0,1,0,0,0,0,1},{1,0,0,0,0,1,0,1,0,0,0,0,1},{1,0,0,0,0,1,0,1,0,0,0,0,1},{0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,1,1,1,0,0,0,1,1,1,0,0}} ),
				_new_pat( _nid(), "2-5 Unnamed", {{0,0,0,0,0,0,0,0,1,1,0,0,1,1,0},{0,0,0,0,0,0,0,0,1,0,0,0,0,1,0},{0,0,0,0,0,0,0,0,0,1,1,1,1,0,0},{0,0,0,0,0,0,1,1,1,0,0,0,1,0,1},{0,0,1,0,0,0,1,0,0,1,0,0,0,1,1},{0,1,0,1,0,0,0,1,0,1,0,0,0,0,0},{1,0,1,0,0,1,0,1,0,1,1,0,0,0,0},{1,0,0,1,1,0,1,0,0,0,1,0,0,0,0},{0,1,0,1,0,0,1,0,1,1,0,0,0,0,0},{1,1,0,1,0,1,0,1,0,0,1,0,0,0,0},{1,0,0,1,0,1,0,1,0,1,1,0,0,0,0},{0,1,1,0,0,0,0,0,0,1,0,0,0,0,0},{0,0,0,1,1,1,1,1,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0,1,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,1,1,0,0,0,0,0,0,0,0,0},{0,0,0,0,1,1,0,0,0,0,0,0,0,0,0}} ),
			},
			{
				_new_pat( _nid(), "3-1 R Pentomino", {{0,1,1},{1,1,0},{0,1,0}} ),
				_new_pat( _nid(), "3-2 Unnamed", {{1,1,0,0,0,0,1,0},{0,1,0,0,0,1,1,1}} ),
				_new_pat( _nid(), "3-3 Unnamed", {{1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1},{1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,1,0,1,0,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0},{0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1},{0,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1},{0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}} ),
				_new_pat( _nid(), "3-4 Unnamed", {{1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,1,1,1,1,1}} ),
				_new_pat( _nid(), "3-5 Unnamed", {{0,1,0,0,0,0,0},{0,0,0,1,0,0,0},{1,1,0,0,1,1,1}} ),
				_new_pat( _nid(), "3-6 Unnamed", {{1,1,1,0,1},{1,0,0,0,0},{0,0,0,1,1},{0,1,1,0,1},{1,0,1,0,1}} ),
				_new_pat( _nid(), "3-6 Unnamed", {{0,0,0,0,0,0,1,0},{0,0,0,0,1,0,1,1},{0,0,0,0,1,0,1,0},{0,0,0,0,1,0,0,0},{0,0,1,0,0,0,0,0},{1,0,1,0,0,0,0,0}} ),
			},
			{
				_new_pat( _nid(), "4-1 Glider", {{0,0,1},{1,0,1},{0,1,1}} ),
				_new_pat( _nid(), "4-2 Light-Weight Spaceship", {{1,0,0,1,0},{0,0,0,0,1},{1,0,0,0,1},{0,1,1,1,1}} ),
			},
			{
				_new_pat( _nid(), "5-1 Gosper's Glider Gun", {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1},{1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}} ),
			}
		}

		for i=1, #tl.cats do
			tl.cur_pats[i] = 1
		end

		_set_size()
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- init
	function toggle_padding()
		toggle_opt(use_padding)
		set_padding(opts[use_padding])
	end

	function set_padding(bool)
		pad = bool == true and (CS < 4 and 0 or 1) or 0
	end

	function rand_cells(rst)
		l_cells=0
		if rst then pause(true)end
		local cc,b=cells[cur],0
		for j=1,GH do
			ccj=cc[j]
			for i=1,GW do
				b=rand()<0.5 and 1or 0
				ccj[i]=b
				l_cells=l_cells+b
			end
		end
	end

	function create_cells()
		local b1,b2,b3={},{},{}
		for j=1,GH do
			b1[j],b2[j],b3[j]={},{},{}
			for i=1,GW do
				b1[j][i],b2[j][i],b3[j][i]=0,0,0
			end
		end
		cells={b1,b2,b3}
		TOT_CELLS=GW*GH
	end

	function fill_grid(fill)
		local cc,ccj=cells[cur]
		for j=1,GH do
			ccj=cc[j]
			for i=1,GW do
				ccj[i]=fill and 1or 0
			end
		end
		l_cells=(fill and GW*GH or 0)
	end

	function set_zoom(dir)
		local z = zoom_lvl
		if dir == 1 then 	z = clamp(z * 2,  1, 8)
		else				z = clamp(z // 2, 1, 8)
		end

		if z ~= zoom_lvl then
			CS = SS//z
			GW = 30*z
			GH = 17*z
			zoom_lvl = z
			set_padding(opts[use_padding])
			gens = 0
			l_cells = 0
			create_cells()
			TOT_CELLS = GW*GH
			if not paused then pause() end
		end
	end

	function init()
		set_padding(opts[use_padding])
		tl.orgn = vec()
		tl.bbox = Rect()
		tl.init_pats()
		create_cells()
		if opts[rand_start] then rand_cells() end

		anim = Animator()

		init_ui()

		ui.tlbar.tls["brush"]:toggle_state()
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- update
	local function ng_wrap()
		cur,pre=pre,cur
		-- the weirdness in here got me over 10ms and 20fps

		local lc,p,c=0,cells[pre],cells[cur]	-- count alive cells, and make buffers local
		local l,r,n,b,cj,pj,pu,pd -- this seems to give me ~2ms (and more token budget)

		for j=1,GH do
			-- make buffer rows local here for faster access
			cj,pj,pu,pd=
				c[j],
				p[j],
				p[(j-1<1 and GH or j-1)],
				p[(j+1>GH and 1 or j+1)]

			for i=1,GW do
				l,r=(i-1<1 and GW or i-1),(i+1>GW and 1or i+1)

				-- count alive neighbors
				n=pu[l]+pu[i]+pu[r]+pj[l]+pj[r]+pd[l]+pd[i]+pd[r]

				b=(n==3or(n==2 and pj[i]==1))and 1or 0
				cj[i]=b
				lc=lc+b
			end
		end
		l_cells=lc
	end

	-- this is considerably slower than the wrapping function
	local function ng_nowrap()
		cur,pre=pre,cur

		local lc,p,c=0,cells[pre],cells[cur]
		local u,d,l,r,n,b,cj,pj,pu,pd

		for j=1,GH do
			u,d=j-1,j+1
			cj,pd,pj,pu=c[j],p[d],p[j],p[u]
			for i=1,GW do
				l,r=i-1,i+1

				n=(pu and pu[l]or 0)
				 +(pu and pu[i]or 0)
				 +(pu and pu[r]or 0)
				 +(pj[l]or 0)
				 +(pj[r]or 0)
				 +(pd and pd[l]or 0)
				 +(pd and pd[i]or 0)
				 +(pd and pd[r]or 0)

				b=(n==3or(n==2 and pj[i]==1))and 1or 0
				cj[i]=b
				lc=lc+b
			end
		end
		l_cells=lc
	end

	local function next_gen()
		if opts[wrap_around] then
			ng_wrap()
		else
			ng_nowrap()
		end
		gens=gens+1
	end

	local function update_tools()
		if tl.type == "rect" then
			ui.icn_r_sqr:set_state(sh_mod)
			ui.icn_r_fill:set_state(ct_mod)
			ui.icn_r_cntr:set_state(al_mod)
		elseif tl.type == "circle" then
			ui.icn_c_sqr:set_state(sh_mod)
			ui.icn_c_fill:set_state(ct_mod)
			ui.icn_c_cntr:set_state(al_mod)
		end
	end

	local function update_ui()
	bma("ui update",function()--@bm
		tc.update(mx, my, lmb, mmb, rmb)
	end)--@bm
	end

	local function update(dt)
	bma("update",function()--@bm
		if cur_scr == GAME_SCR then
			update_tools()
			if use_ibar then ui.lbl_nfo:set_text("") end

			if not mouse_on_ui then
				tl.clear()
				tl.draw_points(g_mx, g_my)
				ui.lbl_mouse:set_text(zoom_lvl.." | "..g_mx..", "..g_my)
			end

			if not paused then
			bma("compute gen",function()--@bm
				next_gen()
			end)--@bm
			end

			ui.lbl_gens:set_text("G: "..gens)
			ui.lbl_cells2:set_text(tostr(l_cells))
			ui.lbl_cells3:set_text(tostr(TOT_CELLS-l_cells))
			ui.lbl_cells4:set_text(tostr(TOT_CELLS))

			anim:update()
			update_ui()
		elseif cur_scr == OPTS_SCR then
			update_ui()
		end
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- render
	local function render_rects()
		local rect,ddcs,s,c=rect,opts[draw_d_cells],cells[sel],cells[cur]
		local rs,ca,cd,cs=CS-pad,cols.alive,cols.dead,(eraser and cols.erasing or cols.selected)
		local sj,cj,x,y
		for j=1,GH do
			y=(j-1)*CS+pad
			sj,cj=s[j],c[j]
			for i=1,GW do
				x=(i-1)*CS+pad
				if     not mouse_on_ui and sj[i]==1 then rect(x,y,rs,rs,cs)
				elseif cj[i]==1 then rect(x,y,rs,rs,ca)
				elseif ddcs then rect(x,y,rs,rs,cd)
				end
			end
		end
		if not tl.is_drawing and not mouse_on_ui then
			rect((g_mx-1)*CS+pad,(g_my-1)*CS+pad,CS-pad,CS-pad,cs)
		end
	end

	local function render_pix()
		local pix,ddcs,s,c=pix,opts[draw_d_cells],cells[sel],cells[cur]
		local ca,cd,cs=cols.alive,cols.dead,(eraser and cols.erasing or cols.selected)
		local sj,cj,x,y
		for j=1,GH do
			y=j-1
			sj,cj=s[j],c[j]
			for i=1,GW do
				x=i-1
				if not mouse_on_ui and sj[i]==1 then pix(x,y,cs)
				elseif cj[i]==1 then pix(x,y,ca)
				elseif ddcs then pix(x,y,cd)
				end
			end
		end
		if not tl.is_drawing and not mouse_on_ui then
			pix(g_mx-1,g_my-1,cs)
		end
	end

	local function render_ui()
	bma("ui_render",function()--@bm
		tc.draw()
	end)--@bm
	end

	local function draw_game()
		if CS > 1 then render_rects()
		else           render_pix()
		end
		render_ui()
	end

	local function draw_help()
		printc("Help ".. cur_scr-1 .."/"..num_help_scrs, nil, 0 , cols.header)
		printc("H >>  ", nil, 16, cols.header, false)
		if cur_scr == HELP_SCR1 then
			prints("Screen ", 1, 1, cols.header)
			prints(help_strs.hstr1:gsub('\t', ''), 0, 2, cols.text, true)
			spr(256,2*8,5*8+5,0,1,0,0,8,5)
		elseif cur_scr == HELP_SCR2 then
			prints("Controls ", 1, 1, cols.header)
			prints(help_strs.hstr2:gsub('\t', ''), 2, 2, cols.text, true)
			prints("Mouse editing ", 1, 12, cols.header)
			prints(help_strs.hstr3:gsub('\t', ''), 2, 13, cols.text, true)
		elseif cur_scr == HELP_SCR3 then
			prints("Tools ", 1, 1, cols.header)
			prints(help_strs.hstr4:gsub('\t', ''), 2, 2, cols.text, true)
			prints("Tool modes ", 1, 7, cols.header)
			prints(help_strs.hstr5:gsub('\t', ''), 2, 8, cols.text, true)
		elseif cur_scr == HELP_SCR4 then
			prints("Pattern Categories ", 1, 2, cols.header)
			prints(help_strs.hstr6:gsub('\t', ''), 2, 3, cols.text, true)
		end
	end

	local function draw_options()
		printc("Options", nil, 0 , cols.header)
		prints("Game", 1, 2, cols.header)
		prints("Startup", 1, 9, cols.header)
		render_ui()
	end

	local function render()
	bma("render",function()--@bm
		cls(cols.bg)
		if cur_scr < HELP_SCR1 then
			draw_game()
		elseif cur_scr < OPTS_SCR then
			draw_help()
		else
			draw_options()
		end
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--


--=--=--=--=--=--=--=--=--=--=--=--=--
-- input
	local function handle_keys()
		local k = keys
		if keyp(k.D) then dbg:toggle()
		else
			if     cur_scr >= OPTS_SCR then
				if keyp(k.O) then toggle_options() end
			elseif cur_scr >= HELP_SCR1 then
				if keyp() or mbtnp() then toggle_help() end
			else
				ct_mod = key(k.CTRL)
				sh_mod = key(k.SHFT)
				al_mod = key(k.ALT)

				tl.mode1 = ct_mod
				tl.mode2 = sh_mod
				tl.mode3 = al_mod

				if keyp(k.SPACE)                    then
					if sh_mod then pause(true)
					else toggle_pause()
					end
				elseif keyp(k.G, 10, 5)             then if paused then next_gen() end
				elseif keyp(k.ENTER)                then rand_cells(opts[rand_reset])
				elseif keyp(k.PGUP)                 then set_zoom(-1)
				elseif keyp(k.PGDN)                 then set_zoom(1)

				elseif keyp(k.O)                    then toggle_options()
				elseif keyp(k.H)                    then toggle_help()
				elseif keyp(k.P)                    then ui.cbx_pad:toggle_state()
				elseif keyp(k.K)                    then ui.cbx_deadc:toggle_state()

				elseif keyp(k.TAB)                  then toggle_ui()
				elseif keyp(k.I)                    then toggle_ibar()
				elseif keyp(k.T)                    then toggle_topbar()

				elseif keyp(k.E)                    then toggle_eraser()
				elseif keyp(k.V)
				or     keyp(k.N1)                   then if tl.type ~= "brush" then ui.tlbar.tls["brush"]:toggle_state() end
				elseif keyp(k.U)                    then ui.tlbar_handle:reset()
				elseif keyp(k.L)                    then if tl.type ~= "line" then ui.tlbar.tls["line"]:toggle_state() end
				elseif keyp(k.R)                    then if tl.type ~= "rect" then ui.tlbar.tls["rect"]:toggle_state() end
				elseif keyp(k.C) and     sh_mod     then fill_grid(false)
				elseif keyp(k.C) and not sh_mod     then if tl.type ~= "circle" then ui.tlbar.tls["circle"]:toggle_state() end
				elseif keyp(k.F) and     sh_mod     then fill_grid(true)
				elseif keyp(k.F) and not sh_mod     then if tl.type ~= "fill" then ui.tlbar.tls["fill"]:toggle_state() end
				elseif keyp(k.W, 10, 5) then
					if     tl.type == "brush"   then tl.expand()
					elseif tl.type == "pattern" then tl.next_pattern()
					end
				elseif keyp(k.S, 10, 5) then
					if     tl.type == "brush"   then tl.contract()
					elseif tl.type == "pattern" then tl.prev_pattern()
					end
				elseif keyp(k.N2) then
					if tl.type ~= "pattern" then ui.tlbar.tls["pattern"]:toggle_state() end
					tl.set_category(1)
				elseif keyp(k.N3) then
					if tl.type ~= "pattern" then ui.tlbar.tls["pattern"]:toggle_state() end
					tl.set_category(2)
				elseif keyp(k.N4) then
					if tl.type ~= "pattern" then ui.tlbar.tls["pattern"]:toggle_state() end
					tl.set_category(3)
				elseif keyp(k.N5) then
					if tl.type ~= "pattern" then ui.tlbar.tls["pattern"]:toggle_state() end
					tl.set_category(4)
				elseif keyp(k.N6) then
					if tl.type ~= "pattern" then ui.tlbar.tls["pattern"]:toggle_state() end
					tl.set_category(5)
				end
			end
		end
	end

	local function handle_mouse()
		update_mouse()
		g_mx = mx//CS+1
		g_my = my//CS+1

		if cur_scr == GAME_SCR and not mouse_on_ui then
			if mbtnp(M2) then
				tl.cancel()
			else
				if tl.type == "brush" then
					if     mbtn(M1) then tl.commit(not eraser)
					elseif mbtn(M2) then tl.commit(false)
					end
				elseif mbtnp(M1) then
					if tl.type == "line" then
						if tl.is_drawing then tl.commit(not eraser) end
						tl.start(g_mx, g_my)
					elseif (tl.type == "fill" or tl.type == "pattern") or tl.is_drawing then
						tl.commit(not eraser)
						tl.cancel()
					else
						tl.toggle(g_mx, g_my)
					end
				elseif mbtnp(M3) then
					tl.commit(false)
					tl.cancel()
				end
			end
		end
	end

	local function input()
	bma("input",function()--@bm
		handle_mouse()
		handle_keys()
	end)--@bm
	end
--=--=--=--=--=--=--=--=--=--=--=--=--

--=--=--=--=--=--=--=--=--=--=--=--=--
-- Unsorted stuff
	function toggle_ui()
		if ui_vis then
			if not anim:is_playing() then
				anim:add_anim("tlbar_show", ui.tlbar_handle, ui.tlbar_handle.x, ui.tlbar_handle.y)
				anim:play("tlbar_hide")
				anim:play("ibar_hide")
				anim:play("topbar_hide")
				ui_vis = false
			end
		else
			if not anim:is_playing() then
				anim:play("tlbar_show")
				anim:play("ibar_show")
				anim:play("topbar_show")
				ui_vis = true
			end
		end
	end

	function toggle_help()
		if cur_scr == GAME_SCR then
			cur_scr = HELP_SCR1
		else
			cur_scr = wrap(cur_scr+1, 1, num_help_scrs+1)--%(num_help_scrs+1)
		end
	end

	function toggle_options()
		-- init_options
		if cur_scr == GAME_SCR then
			cur_scr = OPTS_SCR

			ui.grp_ui:disable()
			ui.grp_opts:enable()
		else
			cur_scr = GAME_SCR
			ui.grp_ui:enable()
			ui.grp_opts:disable()
			_hacky_fix()
		end
	end

	function _hacky_fix()
		-- fixes for btn problems when enabling the ui after leaving options screen
		local tltype = tl.type
		tl.switch("brush") -- fix rect mod btns showing when using circle
		tl.switch(tltype)
		if stopped then -- fix pause btn showing over play btn when stopped
			-- ui.btn_stop:turn_on()
			ui.btn_play:turn_off()
			ui.btn_play:enable()

			ui.btn_pause:turn_off()
			ui.btn_pause:disable()
		elseif paused then
			unpause()
			pause()
		else
			pause()
			unpause()
		end
	end

	function toggle_pause()
		if paused then
			unpause()
		else
			pause()
		end
	end

	function unpause()
		if paused then
			stopped=false
			paused = false
			ui.btn_stop:turn_off()
			ui.btn_play:turn_on()
			ui.btn_play:disable()
			ui.btn_pause:enable()
		end
	end

	function pause(restart)
		if not paused or restart then
			paused = true
			if restart then
				reset()
			else
				ui.btn_pause:turn_off()
				ui.btn_pause:disable()
				ui.btn_play:enable()
			end
		end
	end

	function reset()
		gens = 0
		fill_grid(false)
		stopped=true
		ui.btn_stop:turn_on()
		ui.btn_play:turn_off()
		ui.btn_play:enable()

		ui.btn_pause:turn_off()
		ui.btn_pause:disable()
	end

	function set_speed(dir)

	end
--=--=--=--=--=--=--=--=--=--=--=--=--

function TIC()
	local t_new = time()/1000
	local dt = t_new-t_old
	t_old = t_new
	monitor("dt: ", fmt("%.2f",dt*1000))
	monitor("fps: ", fmt("%d",1//dt))

	input()
	update(dt)
	render()

	-- bma("render_dbg",function()--@bm
		dbg:draw()
	-- end)--@bm
end


init()
