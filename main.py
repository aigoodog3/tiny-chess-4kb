V=(0,100,320,330,500,900,0)
TB=b'iiiiiiiisssssssskkmoomkkjjknnkjjiiimmiiijhgiighjjkkeekkjiiiiiiii_acccca_aeiiiieacikllkiccjlmmljccilmmliccjkllkjcaeijjiea_acccca_'+bytes([105]*64)+bytes([105]*64)+bytes([105]*64)+b'caa__aaccaa__aaccaa__aaccaa__aaceccaaccegeeeeeegmmiiiimmmokiikom'
KN=(-33,-31,-18,-14,14,18,31,33);KG=(-17,-16,-15,-1,1,15,16,17)
DG=(-17,-15,15,17);SG=(-16,-1,1,16);DR={2:KN,3:DG,4:SG,5:KG,6:KG}
START='rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -'
INF=10**6
B=[0]*128;C=1;EP=0;KS=[0,0];U=[];ND=0;RB=0;NB=0;CAP=5500
def pf(f):
 global B,C,EP,KS
 B=[0]*128;KS=[0,0];w=f.split();sq=0
 for ch in w[0]:
  if ch=='/':sq+=8
  elif ch.isdigit():sq+=int(ch)
  else:
   p='PNBRQK'.index(ch.upper())+1
   if ch.islower():p=-p
   B[sq]=p
   if abs(p)==6:KS[p>0]=sq
   sq+=1
 C=1 if w[1]=='w'else -1;EP=(8-int(w[3][1]))*16+(ord(w[3][0])-97)if w[3]!='-'else 0
def atk(s,by):
 Bd=B
 for ds,v in(((15*by,17*by),1),(KN,2),(KG,6)):
  for d in ds:
   t=s+d
   if not t&0x88 and Bd[t]==v*by:return 1
 for ds,a in((DG,3),(SG,4)):
  for d in ds:
   t=s+d
   while not t&0x88:
    p=Bd[t]
    if p:
     if p==a*by or p==5*by:return 1
     break
    t+=d
 return 0
def gen(cap):
 ms=[];ap=ms.append;Bd=B;c=C
 for s in range(128):
  p=Bd[s]
  if p*c<=0:continue
  t=p*c
  if t==1:
   d=-16*c;o=s+d
   for dd in (d-1,d+1):
    u=s+dd
    if not u&0x88 and(Bd[u]*c<0 or u==EP!=0):
     for pr in((5,2,3,4)if not(u>>4)%7 else(0,)):ap(s|u<<7|pr<<14)
   if not Bd[o]:
    if not(o>>4)%7:
     for pr in(5,2,3,4):ap(s|o<<7|pr<<14)
    elif not cap:
     ap(s|o<<7)
     if s>>4 in(1,6)and not Bd[o+d]:ap(s|(o+d)<<7)
  else:
   for d in DR[t]:
    u=s+d
    while not u&0x88:
     q=Bd[u]
     if q*c<0 or(not q and not cap):ap(s|u<<7)
     if q or t in(2,6):break
     u+=d
 return ms
def mk(m):
 global C,EP
 U.append((B[:],C,EP,KS[:]));fr=m&127;to=(m>>7)&127;p=B[fr]
 if abs(p)==1 and to==EP and not B[to] and(fr&7)!=(to&7):B[to+16*C]=0
 B[fr]=0;B[to]=(m>>14&7)*C if(m>>14)&7 else p
 if abs(p)==6:
  KS[C>0]=to
  if abs(to-fr)==2:
   rf,rt=(fr+3,fr+1)if to>fr else(fr-4,fr-1);B[rt]=B[rf];B[rf]=0
 EP=0
 if abs(p)==1 and abs(to-fr)==32:EP=(fr+to)//2
 C=-C
def um():
 global C,EP
 b,c,ep,k=U.pop();B[:]=b;C=c;EP=ep;KS[:]=k
def ev():
 s=0
 for sq in range(128):
  p=B[sq]
  if p:
   t=abs(p);i=sq-(sq>>1&56)
   if p>0:s+=V[t]+(TB[(t-1)*64+i]-105)*5
   else:s-=V[t]+(TB[(t-1)*64+63-i]-105)*5
 return s
def sr(d,a,b,ply):
 global ND,RB
 ND+=1
 if ND&255==0 and ND>NB:raise EOFError
 if ply>90:return C*ev()
 inc=atk(KS[C>0],-C);best=-INF
 if d<=0 and not inc:
  st=C*ev()
  if st>=b:return st
  if st>a:a=st
  best=st
 legal=0
 for m in sorted(gen(d<=0 and not inc),key=lambda m:-V[abs(B[(m>>7)&127])]-(ply==0 and m==RB)*30000):
  mk(m)
  if atk(KS[C<0],C):um();continue
  legal+=1
  if ply==0 and not RB:RB=m
  s=-sr(d-1,-b,-a,ply+1);um()
  if s>best:best=s
  if s>a:
   a=s
   if ply==0:RB=m
  if a>=b:break
 if not legal:return -990000+ply if inc else best if d<=0 else 0
 return best
def think(cap,maxd):
 global ND,RB,NB
 ND=0;NB=cap;mark=len(U);RB=0
 try:
  for d in range(1,maxd+1):sr(d,-INF,INF,0)
 except EOFError:
  while len(U)>mark:um()
 return RB
def main():
 pf(START);z=lambda u:(8-int(u[1]))*16+(ord(u[0])-97)
 while True:
  try:w=input().split()
  except EOFError:break
  if not w:continue
  c=w[0]
  if c=='uci':print('uciok',flush=1)
  elif c=='isready':print('readyok',flush=1)
  elif c=='position':
   if w[1]=='startpos':pf(START);i=2
   else:pf(' '.join(w[2:8]));i=8
   if i<len(w)and w[i]=='moves':
    for u in w[i+1:]:mk(z(u)|z(u[2:])<<7|(('nbrq'.index(u[4])+2 if len(u)>4 else 0)<<14))
  elif c=='go':
   d={w[j]:int(w[j+1])for j in range(1,len(w)-1,2)if w[j+1].lstrip('-').isdigit()}
   m=think(min(CAP,d.get('nodes',CAP)),d.get('depth',64));r=chr(97+(m&7))+str(8-(m>>4&7))+chr(97+(m>>7&7))+str(8-(m>>11&7))if m else'0000'
   if m and(m>>14)&7:r+='nbrq'[(m>>14&7)-2]
   print('bestmove '+r,flush=1)
  elif c=='quit':break
main()
