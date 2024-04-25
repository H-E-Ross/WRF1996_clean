

module module_bl_gwdo
contains

   subroutine gwdo(u3d,v3d,t3d,qv3d,p3d,p3di,pi3d,z,                           &
                  rublten,rvblten,                                             &
                  dtaux3d,dtauy3d,dusfcg,dvsfcg,                               &
                  var2d,oc12d,oa2d1,oa2d2,oa2d3,oa2d4,ol2d1,ol2d2,ol2d3,ol2d4, &
                  sina,cosa,znu,znw,p_top,                                     &
                  cp,g,rd,rv,ep1,pi,                                           &
                  dt,dx,kpbl2d,itimestep,                                      &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte)

   implicit none










































  integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                 &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
  integer,  intent(in   )   ::      itimestep

  real,     intent(in   )   ::      dt,dx,cp,g,rd,rv,ep1,pi

  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(in   )   ::                                           qv3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                          t3d, &
                                                                             z
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(in   )   ::                                           p3di

  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(inout)   ::                                        rublten, &
                                                                      rvblten
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(inout)   ::                                        dtaux3d, &
                                                                      dtauy3d

  real,      dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d

  integer,   dimension( ims:ime, jms:jme )                                   , &
             intent(in  )   ::                                         kpbl2d
  real,   dimension( ims:ime, jms:jme )                                      , &
             intent(inout  )   ::                                      dusfcg, &
                                                                       dvsfcg

  real,   dimension( ims:ime, jms:jme )                                      , &
             intent(in  )   ::                                          var2d, &
                                                                        oc12d, &
                                                      oa2d1,oa2d2,oa2d3,oa2d4, &
                                                      ol2d1,ol2d2,ol2d3,ol2d4, &
                                                                    sina,cosa

  real,     dimension( kms:kme )                                             , &
            optional                                                         , &
            intent(in  )   ::                                             znu, &
                                                                          znw

  real,     optional, intent(in  )   ::                                 p_top



  real,   dimension( its:ite, kts:kte )  ::                           delprsi, &
                                                                          pdh
  real,   dimension( its:ite, kts:kte )  ::  ugeo, vgeo, dudt, dvdt, dtaux, dtauy
  real,   dimension( its:ite )  ::  dusfc, dvsfc
  real,     dimension( its:ite, kts:kte+1 )   ::                         pdhi
  real,   dimension( its:ite, 4 )        ::                               oa4, &
                                                                          ol4
  integer ::  i,j,k,kpblmax

   do k = kts,kte
     if (znu(k).gt.0.6) kpblmax = k + 1
   enddo

   do j = jts,jte
      do k = kts,kte+1
        do i = its,ite
           if (k.le.kte)pdh(i,k) = p3d(i,k,j)
           pdhi(i,k) = p3di(i,k,j)
        enddo
      enddo

      do k = kts,kte
        do i = its,ite
          delprsi(i,k) = pdhi(i,k)-pdhi(i,k+1)

          ugeo(i,k) = u3d(i,k,j)*cosa(i,j) - v3d(i,k,j)*sina(i,j)
          vgeo(i,k) = u3d(i,k,j)*sina(i,j) + v3d(i,k,j)*cosa(i,j)
          dudt(i,k) = 0.0
          dvdt(i,k) = 0.0
        enddo
      enddo
        do i = its,ite
            oa4(i,1) = oa2d1(i,j)
            oa4(i,2) = oa2d2(i,j)
            oa4(i,3) = oa2d3(i,j)
            oa4(i,4) = oa2d4(i,j)
            ol4(i,1) = ol2d1(i,j)
            ol4(i,2) = ol2d2(i,j)
            ol4(i,3) = ol2d3(i,j)
            ol4(i,4) = ol2d4(i,j)
        enddo
      call gwdo2d(dudt=dudt(its,kts),dvdt=dvdt(its,kts)                        &
              ,dtaux2d=dtaux(its,kts),dtauy2d=dtauy(its,kts)                   &
              ,u1=ugeo(its,kts),v1=vgeo(its,kts)                               &
              ,t1=t3d(ims,kms,j),q1=qv3d(ims,kms,j)                            &
              ,del=delprsi(its,kts)                                            &
              ,prsi=pdhi(its,kts)                                              &
              ,prsl=pdh(its,kts),prslk=pi3d(ims,kms,j)                         &
              ,zl=z(ims,kms,j)                                                 &
              ,kpblmax=kpblmax                                                 &
              ,var=var2d(ims,j),oc1=oc12d(ims,j)                               &
              ,oa4=oa4,ol4=ol4                                                 &
              ,dusfc=dusfc(its),dvsfc=dvsfc(its)                               &
              ,g_=g,cp_=cp,rd_=rd,rv_=rv,fv_=ep1,pi_=pi                        &
              ,dxmeter=dx,deltim=dt                                            &
              ,kpbl=kpbl2d(ims,j),lat=j                                        &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
      do k = kts,kte
        do i = its,ite

          rublten(i,k,j) = rublten(i,k,j)+dudt(i,k)*cosa(i,j) + dvdt(i,k)*sina(i,j)
          rvblten(i,k,j) = rvblten(i,k,j)-dudt(i,k)*sina(i,j) + dvdt(i,k)*cosa(i,j)
          dtaux3d(i,k,j) = dtaux(i,k)*cosa(i,j) + dtauy(i,k)*sina(i,j)
          dtauy3d(i,k,j) =-dtaux(i,k)*sina(i,j) + dtauy(i,k)*cosa(i,j)
          if(k.eq.kts)then
             dusfcg(i,j) = dusfc(i)*cosa(i,j) + dvsfc(i)*sina(i,j)
             dvsfcg(i,j) =-dusfc(i)*sina(i,j) + dvsfc(i)*cosa(i,j)
          endif
        enddo
      enddo
   enddo

   end subroutine gwdo


   subroutine gwdo2d(dudt, dvdt, dtaux2d, dtauy2d,                             &
                     u1, v1, t1, q1,                                           &
                     del,                                                      &
                     prsi, prsl, prslk, zl,                                    &
                     kpblmax,                                                  &
                     var, oc1, oa4, ol4, dusfc, dvsfc,                         &
                     g_, cp_, rd_, rv_, fv_, pi_,                              &
                     dxmeter, deltim, kpbl, lat,                               &
                     ids, ide, jds, jde, kds, kde,                             &
                     ims, ime, jms, jme, kms, kme,                             &
                     its, ite, jts, jte, kts, kte)















































   implicit none

   integer                           , intent(in   ) :: lat, kpblmax,          &
                                                        ids, ide, jds, jde,    &
                                                        kds, kde, ims, ime,    &
                                                        jms, jme, kms, kme,    &
                                                        its, ite, jts, jte,    &
                                                        kts, kte
   integer, dimension(ims:ime)       , intent(in   ) :: kpbl
   real                              , intent(in   ) :: g_, pi_, rd_, rv_, fv_,&
                                                        cp_, deltim
   real                              , intent(in   ) :: dxmeter
   real, dimension(its:ite,kts:kte)  , intent(inout) :: dudt, dvdt        
   real, dimension(its:ite,kts:kte)  , intent(  out) :: dtaux2d, dtauy2d  
   real, dimension(its:ite,kts:kte)  , intent(in   ) :: u1, v1
   real, dimension(ims:ime,kms:kme)  , intent(in   ) :: t1, q1, prslk, zl 

   real, dimension(its:ite,kts:kte)  , intent(in   ) :: prsl, del 
   real, dimension(its:ite,kts:kte+1), intent(in   ) :: prsi
   real, dimension(its:ite,4)        , intent(in   ) :: oa4, ol4

   real, dimension(ims:ime)          , intent(in   ) :: var, oc1
   real, dimension(its:ite)          , intent(  out) :: dusfc, dvsfc

   real, parameter      ::  ric     = 0.25    
   real, parameter      ::  dw2min  = 1.
   real, parameter      ::  rimin   = -100.
   real, parameter      ::  bnv2min = 1.0e-5
   real, parameter      ::  efmin   = 0.0
   real, parameter      ::  efmax   = 10.0
   real, parameter      ::  xl      = 4.0e4  
   real, parameter      ::  critac  = 1.0e-5
   real, parameter      ::  gmax    = 1.    
   real, parameter      ::  veleps  = 1.0                                                 
   real, parameter      ::  frc     = 1.0      
   real, parameter      ::  ce      = 0.8     
   real, parameter      ::  cg      = 0.5    
   integer,parameter    ::  kpblmin = 2



   integer              ::  latd,lond
   integer              ::  i,k,lcap,lcapp1,nwd,idir,                          &
                            klcap,kp1,ikount,kk

   real                 ::  fdir,cs,rcsks,                                     &
                            wdir,ti,rdz,temp,tem2,dw2,shr2,bvf2,rdelks,        &
                            wtkbj,tem,gfobnv,hd,fro,rim,temc,tem1,efact,       &
                            temv,dtaux,dtauy

   logical, dimension(its:ite)        :: ldrag, icrilv, flag,kloop1
   real, dimension(its:ite)           :: coefm

   real, dimension(its:ite)           :: taub, xn, yn, ubar, vbar, fr,         &
                                         ulow, rulow, bnv, oa, ol, rhobar,     &
                                         dtfac, brvf, xlinv, delks,delks1,     &
                                         zlowtop,cleff
   real, dimension(its:ite,kts:kte+1) :: taup
   real, dimension(its:ite,kts:kte-1) :: velco
   real, dimension(its:ite,kts:kte)   :: bnv2, usqj, taud, rho, vtk, vtj

   integer, dimension(its:ite)        :: kbl, klowtop
   integer, parameter       ::  mdir=8
   integer, dimension(mdir) :: nwdir
   data nwdir/6,7,5,8,2,3,1,4/



   real, parameter      :: frmax  = 10.
   real, parameter      :: olmin  = 1.0e-5
   real, parameter      :: odmin  = 0.1 
   real, parameter      :: odmax  = 10. 

   real                               :: fbdcd
   real                               :: zblk, tautem
   real                               :: fbdpe, fbdke 
   real, dimension(its:ite)           :: delx, dely
   real, dimension(its:ite,4)         :: dxy4, dxy4p
   real, dimension(4)                 :: ol4p
   real, dimension(its:ite)           :: dxy, dxyp, olp, od
   real, dimension(its:ite,kts:kte+1) :: taufb

   integer, dimension(its:ite)        :: komax
   integer                            :: kblk




   lcap   = kte                                                         
   lcapp1 = lcap + 1                                                 
   fdir   = mdir / (2.0*pi_)



   delx(its:ite) = dxmeter 
   dely(its:ite) = dxmeter
   dxy4(its:ite,1)  = delx(its:ite)
   dxy4(its:ite,2)  = dely(its:ite)
   dxy4(its:ite,3)  = sqrt(delx(its:ite)**2. + dely(its:ite)**2.)
   dxy4(its:ite,4)  = dxy4(its:ite,3)
   dxy4p(its:ite,1) = dxy4(its:ite,2)
   dxy4p(its:ite,2) = dxy4(its:ite,1)
   dxy4p(its:ite,3) = dxy4(its:ite,4)
   dxy4p(its:ite,4) = dxy4(its:ite,3)

   cleff(its:ite) = dxmeter



   ldrag   = .false. ; icrilv = .false. ; flag    = .true.

   klowtop = 0       ; kbl    = 0

   dtaux   = 0.      ; dtauy  = 0.      ; xn      = 0.     ; yn      = 0.  
   ubar    = 0.      ; vbar   = 0.      ; rhobar  = 0.     ; ulow    = 0.
   oa      = 0.      ; ol     = 0.      ; taub    = 0.  

   usqj    = 0.      ; bnv2   = 0.      ; vtj     = 0.     ; vtk     = 0. 
   taup    = 0.      ; taud   = 0.      ; dtaux2d = 0.     ; dtauy2d = 0.

   dtfac   = 1.0     ; xlinv  = 1.0/xl



   komax = 0
   taufb = 0.0

   do k = kts,kte
     do i = its,ite
       vtj(i,k) = t1(i,k)  * (1.+fv_*q1(i,k))
       vtk(i,k) = vtj(i,k) / prslk(i,k)
       rho(i,k) = 1./rd_ * prsl(i,k) / vtj(i,k) 
     enddo
   enddo

   do i = its,ite
     zlowtop(i) = 2. * var(i)
   enddo

   do i = its,ite
     kloop1(i) = .true.
   enddo

   do k = kts+1,kte
     do i = its,ite
       if (kloop1(i).and.zl(i,k)-zl(i,1).ge.zlowtop(i)) then
         klowtop(i) = k+1
         kloop1(i) = .false.
       endif
     enddo
   enddo

   do i = its,ite



     kbl(i) = klowtop(i)
     kbl(i) = max(min(kbl(i),kpblmax),kpblmin)
   enddo



   komax(:) = kbl(:)

   do i = its,ite
     delks(i)  = 1.0 / (prsi(i,1) - prsi(i,kbl(i)))
     delks1(i) = 1.0 / (prsl(i,1) - prsl(i,kbl(i)))
   enddo



   do k = kts,kpblmax
     do i = its,ite
       if (k.lt.kbl(i)) then
         rcsks     = del(i,k) * delks(i)
         rdelks    = del(i,k)  * delks(i)
         ubar(i)   = ubar(i) + rcsks  * u1(i,k)      
         vbar(i)   = vbar(i) + rcsks  * v1(i,k)      
         rhobar(i) = rhobar(i) + rdelks * rho(i,k)   
       endif
     enddo
   enddo






   do i = its,ite                                                       
     wdir  = atan2(ubar(i),vbar(i)) + pi_
     idir  = mod(nint(fdir*wdir),mdir) + 1
     nwd   = nwdir(idir)
     oa(i) = (1-2*int( (nwd-1)/4 )) * oa4(i,mod(nwd-1,4)+1)
     ol(i) = ol4(i,mod(nwd-1,4)+1) 



     ol4p(1) = ol4(i,2)
     ol4p(2) = ol4(i,1)
     ol4p(3) = ol4(i,4)
     ol4p(4) = ol4(i,3)
     olp(i)  = ol4p(mod(nwd-1,4)+1) 



     od(i) = olp(i)/max(ol(i),olmin)
     od(i) = min(od(i),odmax)
     od(i) = max(od(i),odmin)



     dxy(i)  = dxy4(i,MOD(nwd-1,4)+1)
     dxyp(i) = dxy4p(i,MOD(nwd-1,4)+1)
   enddo



   do k = kts,kte-1                                                     
     do i = its,ite                                                     
       ti        = 2.0 / (t1(i,k)+t1(i,k+1))                                
       rdz       = 1./(zl(i,k+1) - zl(i,k))
       tem1      = u1(i,k) - u1(i,k+1)
       tem2      = v1(i,k) - v1(i,k+1)   
       dw2       = tem1*tem1 + tem2*tem2
       shr2      = max(dw2,dw2min) * rdz * rdz
       bvf2      = g_*(g_/cp_+rdz*(vtj(i,k+1)-vtj(i,k))) * ti                
       usqj(i,k) = max(bvf2/shr2,rimin)                            
       bnv2(i,k) = 2.0*g_*rdz*(vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
     enddo                                                          
   enddo                                                             



   do i = its,ite                                                       
     ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
     rulow(i) = 1./ulow(i)
   enddo                                                             

   do k = kts,kte-1                                                    
     do i = its,ite                                                   
       velco(i,k) = 0.5 * ((u1(i,k)+u1(i,k+1)) * ubar(i)                       &
                          + (v1(i,k)+v1(i,k+1)) * vbar(i))                 
       velco(i,k) = velco(i,k) * rulow(i)                               
       if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
         velco(i,k) = veleps                                      
       endif
     enddo                                                          
   enddo                                                             



   do i = its,ite                                                       
     ldrag(i) = velco(i,1).le.0.                                    
   enddo                                                             



   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. velco(i,k).le.0.
     enddo                                                          
   enddo                                                             






   do i = its,ite                                                       
     wtkbj     = (prsl(i,1)-prsl(i,2)) * delks1(i)
     bnv2(i,1) = wtkbj * bnv2(i,1)                                
     usqj(i,1) = wtkbj * usqj(i,1)                                
   enddo                                                             

   do k = kpblmin,kpblmax                                                
     do i = its,ite                                                    
       if (k .lt. kbl(i)) then
         rdelks    = (prsl(i,k)-prsl(i,k+1)) * delks1(i)
         bnv2(i,1) = bnv2(i,1) + bnv2(i,k) * rdelks
         usqj(i,1) = usqj(i,1) + usqj(i,k) * rdelks
       endif
     enddo                                                          
   enddo                                                             

   do i = its,ite                                                       
     ldrag(i) = ldrag(i) .or. bnv2(i,1).le.0.0                         
     ldrag(i) = ldrag(i) .or. ulow(i).eq.1.0                           
     ldrag(i) = ldrag(i) .or. var(i) .le. 0.0
   enddo                                                             



   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) usqj(i,k) = usqj(i,1)
     enddo                                                          
   enddo                                                             

   do i = its,ite 
     if (.not.ldrag(i))   then   
       bnv(i) = sqrt( bnv2(i,1) )                                  
       fr(i) = bnv(i)  * rulow(i) * var(i) * od(i)
       fr(i) = min(fr(i),frmax)
       xn(i)  = ubar(i) * rulow(i)
       yn(i)  = vbar(i) * rulow(i)
     endif
   enddo






   do i = its,ite                                                       
     if (.not. ldrag(i))   then   
       efact    = (oa(i) + 2.) ** (ce*fr(i)/frc)                         
       efact    = min( max(efact,efmin), efmax )                            
       coefm(i) = (1. + ol(i)) ** (oa(i)+1.)                   
       xlinv(i) = coefm(i) / cleff(i)
       tem      = fr(i) * fr(i) * oc1(i)
       gfobnv   = gmax * tem / ((tem + cg)*bnv(i))   
       taub(i)  = xlinv(i) * rhobar(i) * ulow(i) * ulow(i)                     &
                * ulow(i) * gfobnv * efact          
     else                                                          
       taub(i) = 0.0                                             
       xn(i)   = 0.0                                             
       yn(i)   = 0.0                                             
     endif                                                         
   enddo                                                             



   do k = kts,kpblmax
     do i = its,ite
       if (k .le. kbl(i)) taup(i,k) = taub(i)
     enddo
   enddo

   do k = kpblmin, kte-1                   
     kp1 = k + 1
     do i = its,ite





       if (k .ge. kbl(i)) then
         icrilv(i) = icrilv(i) .or. ( usqj(i,k) .lt. ric)                      &
                               .or. (velco(i,k) .le. 0.0)
         brvf(i) = max(bnv2(i,k),bnv2min) 
         brvf(i) = sqrt(brvf(i))          
       endif
     enddo

     do i = its,ite
       if (k .ge. kbl(i) .and. (.not. ldrag(i)))   then   
         if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
           temv = 1.0 / velco(i,k)
           tem1 = coefm(i)/dxy(i)*(rho(i,kp1)+rho(i,k))*brvf(i)*velco(i,k)*0.5
           hd   = sqrt(taup(i,k) / tem1)
           fro  = brvf(i) * hd * temv



           tem2 = sqrt(usqj(i,k))
           tem  = 1. + tem2 * fro
           rim  = usqj(i,k) * (1.-fro) / (tem * tem)




           if (rim .le. ric) then  
             if ((oa(i) .le. 0.).or.(kp1 .ge. kpblmin )) then
               temc = 2.0 + 1.0 / tem2
               hd   = velco(i,k) * (2.*sqrt(temc)-temc) / brvf(i)
               taup(i,kp1) = tem1 * hd * hd
             endif
           else                    
             taup(i,kp1) = taup(i,k)
           endif
         endif
       endif
     enddo      
   enddo

   if (lcap.lt.kte) then                                               
     do klcap = lcapp1,kte                                          
       do i = its,ite                                                 
         taup(i,klcap) = prsi(i,klcap) / prsi(i,lcap) * taup(i,lcap)      
       enddo                                                       
     enddo                                                          
   endif                                                             
   do i = its,ite
     if (.not.ldrag(i)) then



       kblk = 0
       fbdpe = 0.0
       fbdke = 0.0
       do k = kte, kpblmin, -1
         if (kblk.eq.0 .and. k.le.kbl(i)) then
           fbdpe = fbdpe + bnv2(i,k)*(zl(i,kbl(i))-zl(i,k))                    &
                   *del(i,k)/g_/rho(i,k)
           fbdke = 0.5*(u1(i,k)**2.+v1(i,k)**2.)



           if (fbdpe.ge.fbdke) then
             kblk = k
             kblk = min(kblk,kbl(i))
             zblk = zl(i,kblk)-zl(i,kts)
           endif
         endif
       enddo
       if (kblk.ne.0) then



         fbdcd = max(2.0-1.0/od(i),0.0)
         taufb(i,kts) = 0.5*rhobar(i)*coefm(i)/dxmeter**2*fbdcd*dxyp(i)        &
                        *olp(i)*zblk*ulow(i)**2
         tautem = taufb(i,kts)/real(kblk-kts)
         do k = kts+1, kblk
           taufb(i,k) = taufb(i,k-1) - tautem
         enddo



         taup(i,:) = taup(i,:) + taufb(i,:)
       endif
     endif
   enddo 



   do k = kts,kte                                                       
     do i = its,ite                                                       
       taud(i,k) = 1. * (taup(i,k+1) - taup(i,k)) * g_ / del(i,k)
     enddo                                                             
   enddo                                                             





   do k = kts,kpblmax-1                                                    
     do i = its,ite                                                    
       if (k .le. kbl(i)) then
         if (taud(i,k).ne.0.)                                                   &
           dtfac(i) = min(dtfac(i),abs(velco(i,k)/(deltim*taud(i,k))))  
       endif
     enddo                                                          
   enddo                                                             

   do i = its,ite
     dusfc(i) = 0.
     dvsfc(i) = 0.
   enddo

   do k = kts,kte                                                       
     do i = its,ite 
       taud(i,k) = taud(i,k) * dtfac(i)                              
       dtaux = taud(i,k) * xn(i)
       dtauy = taud(i,k) * yn(i)
       dtaux2d(i,k) = dtaux
       dtauy2d(i,k) = dtauy
       dudt(i,k) = dtaux + dudt(i,k)
       dvdt(i,k) = dtauy + dvdt(i,k)
       dusfc(i)  = dusfc(i) + dtaux * del(i,k)
       dvsfc(i)  = dvsfc(i) + dtauy * del(i,k)
     enddo                                                          
   enddo                                                             

   do i = its,ite
     dusfc(i) = (-1./g_) * dusfc(i)
     dvsfc(i) = (-1./g_) * dvsfc(i)
   enddo

   return                                                            
   end subroutine gwdo2d


end module module_bl_gwdo
