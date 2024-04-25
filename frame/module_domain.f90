
























MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_configure
   USE module_wrf_error
   USE module_utility
   USE module_domain_type

   
   
   
   

   
   
   
   
   

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   
   
   
   

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level

   
   TYPE(domain), POINTER :: current_grid
   LOGICAL, SAVE :: current_grid_set = .FALSE.

   
   PRIVATE domain_time_test_print
   PRIVATE test_adjust_io_timestr

   INTERFACE get_ijk_from_grid
     MODULE PROCEDURE get_ijk_from_grid1, get_ijk_from_grid2
   END INTERFACE

   INTEGER, PARAMETER :: max_hst_mods = 1000

CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

    END SELECT data_ordering


    RETURN
   END SUBROUTINE adjust_domain_dims_for_move

   SUBROUTINE get_ijk_from_grid1 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid1

   SUBROUTINE get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids = grid%sd31 ; ide = grid%ed31 ; jds = grid%sd32 ; jde = grid%ed32 ; kds = grid%sd33 ; kde = grid%ed33 ;
           ims = grid%sm31 ; ime = grid%em31 ; jms = grid%sm32 ; jme = grid%em32 ; kms = grid%sm33 ; kme = grid%em33 ;
           ips = grid%sp31 ; ipe = grid%ep31 ; jps = grid%sp32 ; jpe = grid%ep32 ; kps = grid%sp33 ; kpe = grid%ep33 ; 
       CASE  ( DATA_ORDER_YXZ )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd33  ; kde = grid%ed33  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm33  ; kme = grid%em33  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp33  ; kpe = grid%ep33  ; 
       CASE  ( DATA_ORDER_ZXY )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_ZYX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd32  ; jde = grid%ed32  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm32  ; jme = grid%em32  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp32  ; jpe = grid%ep32  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_XZY )
           ids = grid%sd31  ; ide = grid%ed31  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm31  ; ime = grid%em31  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp31  ; ipe = grid%ep31  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
       CASE  ( DATA_ORDER_YZX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid2




   SUBROUTINE get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid




   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
















































   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent



   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )


   bdy_mask = .true.     



   sp1 = sd1 ; sp2 = sd2 ; sp3 = sd3
   ep1 = ed1 ; ep2 = ed2 ; ep3 = ed3
   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
         sm1  = sp1 - bdx ; em1 = ep1 + bdx
         sm2  = sp2 - bdy ; em2 = ep2 + bdy
         sm3  = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_YXZ )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_ZXY )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_ZYX )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdy ; em2 = ep2 + bdy
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
      CASE ( DATA_ORDER_XZY )
         sm1 = sp1 - bdx ; em1 = ep1 + bdx
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_YZX )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
   END SELECT
   sm1x = sm1       ; em1x = em1    
   sm2x = sm2       ; em2x = em2
   sm3x = sm3       ; em3x = em3
   sm1y = sm1       ; em1y = em1    
   sm2y = sm2       ; em2y = em2
   sm3y = sm3       ; em3y = em3

   sp1x = sp1 ; ep1x = ep1 ; sp2x = sp2 ; ep2x = ep2 ; sp3x = sp3 ; ep3x = ep3
   sp1y = sp1 ; ep1y = ep1 ; sp2y = sp2 ; ep2y = ep2 ; sp3y = sp3 ; ep3y = ep3


   RETURN
   END SUBROUTINE wrf_patch_domain

   SUBROUTINE alloc_and_configure_domain ( domain_id , active_this_task, grid , parent, kid )









































      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: domain_id
      LOGICAL , OPTIONAL, INTENT(IN)  :: active_this_task 
      TYPE( domain ) , POINTER        :: grid
      TYPE( domain ) , POINTER        :: parent
      INTEGER , INTENT(IN)            :: kid    

      
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny
      LOGICAL :: active


      active = .TRUE.
      IF ( PRESENT( active_this_task ) ) THEN
         active = active_this_task
      ENDIF






      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering

      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: ', &
          'Incorrect value for num_time_levels ', num_time_levels
        CALL wrf_error_fatal3("<stdin>",598,&
TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF


      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE( new_grid%head_statevars )
      new_grid%head_statevars%Ndim = 0
      NULLIFY( new_grid%head_statevars%next)
      new_grid%tail_statevars => new_grid%head_statevars 

      ALLOCATE ( new_grid%parents( max_parents ) ) 
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      new_grid%domain_clock_created = .FALSE.
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO
      new_grid%time_set = .FALSE.
      new_grid%is_intermediate = .FALSE.
      new_grid%have_displayed_alloc_stats = .FALSE.

      new_grid%tiling_latch = .FALSE.  

      
      
      
      
      

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 
      new_grid%active_this_task = active

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         









        
      new_grid%active_this_task = active
      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. , active,     &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sp1,  ep1,  sp2,  ep2,  sp3,  ep3,  &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                               sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   
      )







      new_grid%stepping_to_time = .FALSE.
      new_grid%adaptation_domain = 1
      new_grid%last_step_updated = -1



      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           



      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)




      grid => new_grid

 

      IF ( grid%active_this_task ) THEN

        ALLOCATE( grid%lattsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%lontsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%nametsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%desctsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%itsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%jtsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%id_tsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%ts_filename( grid%max_ts_locs ) )
        grid%ntsloc        = 0
        grid%ntsloc_domain = 0


        ALLOCATE( grid%track_time_in( grid%track_loc_in ) )
        ALLOCATE( grid%track_lat_in( grid%track_loc_in ) )
        ALLOCATE( grid%track_lon_in( grid%track_loc_in ) )
  
        ALLOCATE( grid%track_time_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_lat_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_lon_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_i( grid%track_loc_in ) )
        ALLOCATE( grid%track_j( grid%track_loc_in ) )

      grid%track_loc        = 0
      grid%track_loc_domain = 0
      grid%track_have_calculated = .FALSE.
      grid%track_have_input      = .FALSE.
      ELSE
        WRITE (wrf_err_message,*)"Not allocating time series storage for domain ",domain_id," on this set of tasks"
        CALL wrf_message(TRIM(wrf_err_message))
      ENDIF


      grid%interp_mp = .true.

   END SUBROUTINE alloc_and_configure_domain

   SUBROUTINE get_fieldstr(ix,c,instr,outstr,noutstr,noerr)
     IMPLICIT NONE
     INTEGER, INTENT(IN)          :: ix
     CHARACTER*(*), INTENT(IN)    :: c
     CHARACTER*(*), INTENT(IN)    :: instr
     CHARACTER*(*), INTENT(OUT)   :: outstr
     INTEGER,       INTENT(IN)    :: noutstr  
     LOGICAL,       INTENT(INOUT) :: noerr     
     
     INTEGER, PARAMETER :: MAX_DEXES = 1000
     INTEGER I, PREV, IDEX
     INTEGER DEXES(MAX_DEXES)
     outstr = ""
     prev = 1
     dexes(1) = 1
     DO i = 2,MAX_DEXES
       idex = INDEX(instr(prev:LEN(TRIM(instr))),c)
       IF ( idex .GT. 0 ) THEN
         dexes(i) = idex+prev
         prev = dexes(i)+1
       ELSE
         dexes(i) = LEN(TRIM(instr))+2
       ENDIF
     ENDDO

     IF     ( (dexes(ix+1)-2)-(dexes(ix)) .GT. noutstr ) THEN
       noerr = .FALSE.  
     ELSE IF( dexes(ix) .EQ. dexes(ix+1) ) THEN 
       noerr = .FALSE.  
     ELSE
       outstr = instr(dexes(ix):(dexes(ix+1)-2))
       noerr = noerr .AND. .TRUE.
     ENDIF
   END SUBROUTINE get_fieldstr

   SUBROUTINE change_to_lower_case(instr,outstr)
     CHARACTER*(*) ,INTENT(IN)  :: instr
     CHARACTER*(*) ,INTENT(OUT) :: outstr

     CHARACTER*1                :: c
     INTEGER       ,PARAMETER   :: upper_to_lower =IACHAR('a')-IACHAR('A')
     INTEGER                    :: i,n,n1

     outstr = ' '
     N = len(instr)
     N1 = len(outstr)
     N = MIN(N,N1)
     outstr(1:N) = instr(1:N)
     DO i=1,N
       c = instr(i:i)
       if('A'<=c .and. c <='Z') outstr(i:i)=achar(iachar(c)+upper_to_lower)
     ENDDO
     RETURN
   END SUBROUTINE change_to_lower_case


   SUBROUTINE modify_io_masks1 ( grid , id )
      IMPLICIT NONE

      INTEGER              , INTENT(IN  )  :: id
      TYPE(domain), POINTER                :: grid
      
      TYPE(fieldlist), POINTER :: p, q
      INTEGER, PARAMETER :: read_unit = 10
      LOGICAL, EXTERNAL  :: wrf_dm_on_monitor
      CHARACTER*8000     :: inln, t1, fieldlst
      CHARACTER*256      :: fname, mess, dname, lookee
      CHARACTER*1        :: op, strmtyp
      CHARACTER*3        :: strmid
      CHARACTER*10       :: strmtyp_name
      INTEGER            :: io_status
      INTEGER            :: strmtyp_int, count_em
      INTEGER            :: lineno, fieldno, istrm, retval, itrace
      LOGICAL            :: keepgoing, noerr, gavewarning, ignorewarning, found
      LOGICAL, SAVE      :: you_warned_me = .FALSE.
      LOGICAL, SAVE      :: you_warned_me2(max_hst_mods,max_domains) = .FALSE.

      gavewarning = .FALSE.

      CALL nl_get_iofields_filename( id, fname )

      IF ( grid%is_intermediate ) RETURN                
      IF ( TRIM(fname) .EQ. "NONE_SPECIFIED" ) RETURN   

      IF ( wrf_dm_on_monitor() ) THEN
        OPEN ( UNIT   = read_unit    ,      &
               FILE   = TRIM(fname)      ,      &
               FORM   = "FORMATTED"      ,      &
               STATUS = "OLD"            ,      &
               IOSTAT = io_status         )
        IF ( io_status .EQ. 0 ) THEN   
          keepgoing = .TRUE.
          lineno = 0
          count_em = 0    
          DO WHILE ( keepgoing )
            READ(UNIT=read_unit,FMT='(A)',IOSTAT=io_status) inln
            keepgoing = (io_status .EQ. 0) .AND. (LEN(TRIM(inln)) .GT. 0)  
            IF ( keepgoing ) THEN
              lineno = lineno + 1
              IF ( .NOT. LEN(TRIM(inln)) .LT. LEN(inln) ) THEN
                WRITE(mess,*)'W A R N I N G : Line ',lineno,' of ',TRIM(fname),' is too long. Limit is ',LEN(inln),' characters.' 
                gavewarning = .TRUE.
              ENDIF
              IF ( INDEX(inln,'#') .EQ. 0 ) THEN   
                IF ( keepgoing ) THEN
                  noerr = .TRUE.
                  CALL get_fieldstr(1,':',inln,op,1,noerr)          
                  IF ( TRIM(op) .NE. '+' .AND. TRIM(op) .NE. '-' ) THEN
                    WRITE(mess,*)'W A R N I N G : unknown operation ',TRIM(op),' (should be + or -). Line ',lineno
                    gavewarning = .TRUE.
                  ENDIF
                  CALL get_fieldstr(2,':',inln,t1,1,noerr)          
                  CALL change_to_lower_case(t1,strmtyp) 

                  SELECT CASE (TRIM(strmtyp))
                  CASE ('h')
                     strmtyp_name = 'history'
                     strmtyp_int  = first_history
                  CASE ('i')
                     strmtyp_name = 'input'
                     strmtyp_int  = first_input
                  CASE DEFAULT
                     WRITE(mess,*)'W A R N I N G : unknown stream type ',TRIM(strmtyp),'. Line ',lineno
                     gavewarning = .TRUE.
                  END SELECT

                  CALL get_fieldstr(3,':',inln,strmid,3,noerr)      
                  READ(strmid,'(I3)') istrm
                  IF ( istrm .LT. 0 .OR. istrm .GT. last_history ) THEN
                    WRITE(mess,*)'W A R N I N G : invalid stream id ',istrm,' (should be 0 <= id <= ',last_history,'). Line ',lineno
                    gavewarning = .TRUE.
                  ENDIF
                  CALL get_fieldstr(4,':',inln,fieldlst,8000,noerr) 
                  IF ( noerr ) THEN
                    fieldno = 1
                    CALL get_fieldstr(fieldno,',',fieldlst,t1,8000,noerr)
                    CALL change_to_lower_case(t1,lookee)
                    DO WHILE ( noerr )    
                      p => grid%head_statevars%next
                      found = .FALSE.
                      count_em = count_em + 1
                      DO WHILE ( ASSOCIATED( p ) )
  
                        IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
  
                          DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                            CALL change_to_lower_case( p%dname_table( grid%id, itrace ) , dname ) 

                            IF ( TRIM(dname) .EQ. TRIM(lookee) ) &
                            CALL warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                                      strmtyp_name, dname, fname, lookee,      &
                                                      p%streams_table(grid%id,itrace)%stream,  &
                                                      mess, found, you_warned_me2)
                          ENDDO
                        ELSE 
                          IF ( p%Ntl .GT. 0 ) THEN
                            CALL change_to_lower_case(p%DataName(1:LEN(TRIM(p%DataName))-2),dname)
                          ELSE
                            CALL change_to_lower_case(p%DataName,dname)
                          ENDIF
  
                          IF ( TRIM(dname) .EQ. TRIM(lookee) ) &
                          CALL warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                                    strmtyp_name, dname, fname, lookee,      &
                                                    p%streams, mess, found, you_warned_me2)
                        ENDIF
                        p => p%next
                      ENDDO
                      IF ( .NOT. found ) THEN
                        WRITE(mess,*)'W A R N I N G : Unable to modify mask for ',TRIM(lookee),&
                                     '.  Variable not found. File: ',TRIM(fname),' at line ',lineno
                        CALL wrf_message(mess)
                        gavewarning = .TRUE.
                      ENDIF
                      fieldno = fieldno + 1
                      CALL get_fieldstr(fieldno,',',fieldlst,t1,256,noerr)
                      CALL change_to_lower_case(t1,lookee)
                    ENDDO
                  ELSE
                    WRITE(mess,*)'W A R N I N G : Problem reading ',TRIM(fname),' at line ',lineno
                    CALL wrf_message(mess)
                    gavewarning = .TRUE.
                  ENDIF
                ENDIF  
              ENDIF    
            ENDIF      
          ENDDO
        ELSE
          WRITE(mess,*)'W A R N I N G : Problem opening ',TRIM(fname)
          CALL wrf_message(mess)
          gavewarning = .TRUE.
        ENDIF
        CLOSE( read_unit )
        IF ( gavewarning ) THEN
          CALL nl_get_ignore_iofields_warning(1,ignorewarning)
          IF ( .NOT. ignorewarning ) THEN
            CALL wrf_message(mess)
            WRITE(mess,*)'modify_io_masks: problems reading ',TRIM(fname) 
            CALL wrf_message(mess)
            CALL wrf_error_fatal3("<stdin>",1098,&
'Set ignore_iofields_warn to true in namelist to ignore')
          ELSE
            IF ( .NOT. you_warned_me ) THEN
              if ( .NOT. you_warned_me2(count_em,id) ) CALL wrf_message(mess)  
              WRITE(mess,*)'Ignoring problems reading ',TRIM(fname) 
              CALL wrf_message(mess)
              CALL wrf_message('Continuing.  To make this a fatal error, set ignore_iofields_warn to false in namelist' )
              CALL wrf_message(' ')
              you_warned_me = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF  

      
   END SUBROUTINE modify_io_masks1

   SUBROUTINE warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                   strmtyp_name, dname, fname, lookee,      &
                                   p_stream, mess, found, you_warned_me2)

      IMPLICIT NONE






     INTEGER,       INTENT(IN )   :: id, istrm, lineno, strmtyp_int
     INTEGER,       INTENT(IN )   :: p_stream(*), count_em
     CHARACTER*1,   INTENT(IN )   :: op
     CHARACTER*10,  INTENT(IN )   :: strmtyp_name
     CHARACTER*256, INTENT(IN )   :: dname, fname, lookee
     CHARACTER*256, INTENT(OUT)   :: mess
     LOGICAL,       INTENT(OUT)   :: found
     LOGICAL,       INTENT(INOUT) :: you_warned_me2(max_hst_mods,max_domains)
   
     INTEGER                      :: retval

     found = .TRUE.
     IF      ( TRIM(op) .EQ. '+' ) THEN
       CALL get_mask( p_stream, strmtyp_int + istrm - 1, retval )
       IF ( retval .NE. 0 ) THEN
         WRITE(mess,*) 'Domain ',id, ' W A R N I N G : Variable ',TRIM(lookee),' already on ', &
                       TRIM(strmtyp_name), ' stream ',istrm, '.  File: ', TRIM(fname),' at line ',lineno
       ELSE
         WRITE(mess,*) 'Domain ', id, ' Setting ', TRIM(strmtyp_name), ' stream ',istrm,' for ', &
                                  TRIM(DNAME)  ; CALL wrf_debug(1,mess)
         CALL set_mask( p_stream, strmtyp_int + istrm - 1 )
       ENDIF
     ELSE IF ( TRIM(op) .EQ. '-' ) THEN
       CALL get_mask( p_stream, strmtyp_int + istrm - 1, retval )
       IF ( retval .EQ. 0 ) THEN
         WRITE(mess,*) 'Domain ',id, ' W A R N I N G : Variable ',TRIM(lookee),' already off ', &
                       TRIM(strmtyp_name), ' stream ',istrm, '. File: ',TRIM(fname),' at line ',lineno
       ELSE
         WRITE(mess,*) 'Domain ', id, ' Resetting ', TRIM(strmtyp_name), ' stream ',istrm,' for ', &
                                    TRIM(DNAME)  ; CALL wrf_debug(1,mess) 
         CALL reset_mask( p_stream, strmtyp_int + istrm - 1)
       ENDIF
     ENDIF
     IF ( count_em > max_hst_mods ) THEN
       WRITE(mess,*)'ERROR module_domain:  Array size for you_warned_me2 is fixed at ',max_hst_mods
       CALL wrf_message(mess)
       CALL wrf_error_fatal3("<stdin>",1163,&
'Did you really type > max_hst_mods fields into ', TRIM(fname) ,' ?')
     ELSE
       IF ( .NOT. you_warned_me2(count_em,id) ) THEN
         CALL wrf_message(mess)     
         you_warned_me2(count_em,id) = .TRUE.
       ENDIF
     ENDIF

   END SUBROUTINE warn_me_or_set_mask 







   SUBROUTINE alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_alloc_space_0, ONLY : alloc_space_field_core_0
      USE module_alloc_space_1, ONLY : alloc_space_field_core_1
      USE module_alloc_space_2, ONLY : alloc_space_field_core_2
      USE module_alloc_space_3, ONLY : alloc_space_field_core_3
      USE module_alloc_space_4, ONLY : alloc_space_field_core_4
      USE module_alloc_space_5, ONLY : alloc_space_field_core_5
      USE module_alloc_space_6, ONLY : alloc_space_field_core_6
      USE module_alloc_space_7, ONLY : alloc_space_field_core_7
      USE module_alloc_space_8, ONLY : alloc_space_field_core_8
      USE module_alloc_space_9, ONLY : alloc_space_field_core_9

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in

      
      INTEGER(KIND=8)  num_bytes_allocated
      INTEGER  idum1, idum2

      IF ( grid%id .EQ. 1 ) CALL wrf_message ( &
          'DYNAMICS OPTION: Eulerian Mass Coordinate ')

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 

      
      CALL alloc_space_field_core_0 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated , &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_1 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_2 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_3 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_4 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_5 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_6 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_7 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_8 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_9 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )

      IF ( .NOT. grid%have_displayed_alloc_stats ) THEN
        
        
        WRITE(wrf_err_message,*)&
            'alloc_space_field: domain ',id,', ',num_bytes_allocated,' bytes allocated'
        CALL  wrf_debug( 0, wrf_err_message )
        grid%have_displayed_alloc_stats = .TRUE.   
      ENDIF


      grid%alloced_sd31=sd31
      grid%alloced_ed31=ed31
      grid%alloced_sd32=sd32
      grid%alloced_ed32=ed32
      grid%alloced_sd33=sd33
      grid%alloced_ed33=ed33
      grid%alloced_sm31=sm31
      grid%alloced_em31=em31
      grid%alloced_sm32=sm32
      grid%alloced_em32=em32
      grid%alloced_sm33=sm33
      grid%alloced_em33=em33
      grid%alloced_sm31x=sm31x
      grid%alloced_em31x=em31x
      grid%alloced_sm32x=sm32x
      grid%alloced_em32x=em32x
      grid%alloced_sm33x=sm33x
      grid%alloced_em33x=em33x
      grid%alloced_sm31y=sm31y
      grid%alloced_em31y=em31y
      grid%alloced_sm32y=sm32y
      grid%alloced_em32y=em32y
      grid%alloced_sm33y=sm33y
      grid%alloced_em33y=em33y

      grid%allocated=.TRUE.

   END SUBROUTINE alloc_space_field

   
   
   
   
   

   SUBROUTINE ensure_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in
      LOGICAL                         :: size_changed

      size_changed=         .not. ( &
         grid%alloced_sd31 .eq. sd31 .and. grid%alloced_ed31 .eq. ed31 .and. &
         grid%alloced_sd32 .eq. sd32 .and. grid%alloced_ed32 .eq. ed32 .and. &
         grid%alloced_sd33 .eq. sd33 .and. grid%alloced_ed33 .eq. ed33 .and. &
         grid%alloced_sm31 .eq. sm31 .and. grid%alloced_em31 .eq. em31 .and. &
         grid%alloced_sm32 .eq. sm32 .and. grid%alloced_em32 .eq. em32 .and. &
         grid%alloced_sm33 .eq. sm33 .and. grid%alloced_em33 .eq. em33 .and. &
         grid%alloced_sm31x .eq. sm31x .and. grid%alloced_em31x .eq. em31x .and. &
         grid%alloced_sm32x .eq. sm32x .and. grid%alloced_em32x .eq. em32x .and. &
         grid%alloced_sm33x .eq. sm33x .and. grid%alloced_em33x .eq. em33x .and. &
         grid%alloced_sm31y .eq. sm31y .and. grid%alloced_em31y .eq. em31y .and. &
         grid%alloced_sm32y .eq. sm32y .and. grid%alloced_em32y .eq. em32y .and. &
         grid%alloced_sm33y .eq. sm33y .and. grid%alloced_em33y .eq. em33y &
      )
      if(.not. grid%allocated .or. size_changed) then
         if(.not. grid%allocated) then
            call wrf_debug(1,'ensure_space_field: calling alloc_space_field because a grid was not allocated.')
         else
            if(size_changed) &
                 call wrf_debug(1,'ensure_space_field: deallocating and reallocating a grid because grid size changed.')
         end if
         if(grid%allocated) &
              call dealloc_space_field( grid )
         call alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )
      end if

   END SUBROUTINE ensure_space_field






   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: id

      

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found

      

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      
      
      

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL domain_destroy( grid )
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: ', &
           'dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal3("<stdin>",1475,&
TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain








   SUBROUTINE domain_destroy ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain) , POINTER          :: grid

      CALL dealloc_space_field ( grid )
      CALL dealloc_linked_lists( grid )
      DEALLOCATE( grid%parents )
      DEALLOCATE( grid%nests )
      
      CALL domain_clock_destroy( grid )
      CALL domain_alarms_destroy( grid )
      IF ( ASSOCIATED( grid%i_start ) ) THEN
        DEALLOCATE( grid%i_start ) 
      ENDIF
      IF ( ASSOCIATED( grid%i_end ) ) THEN
        DEALLOCATE( grid%i_end )
      ENDIF
      IF ( ASSOCIATED( grid%j_start ) ) THEN
        DEALLOCATE( grid%j_start )
      ENDIF
      IF ( ASSOCIATED( grid%j_end ) ) THEN
        DEALLOCATE( grid%j_end )
      ENDIF
      IF ( ASSOCIATED( grid%itsloc ) ) THEN
        DEALLOCATE( grid%itsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%jtsloc ) ) THEN
        DEALLOCATE( grid%jtsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%id_tsloc ) ) THEN
        DEALLOCATE( grid%id_tsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lattsloc ) ) THEN
        DEALLOCATE( grid%lattsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lontsloc ) ) THEN
        DEALLOCATE( grid%lontsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%nametsloc ) ) THEN
        DEALLOCATE( grid%nametsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%desctsloc ) ) THEN
        DEALLOCATE( grid%desctsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%ts_filename ) ) THEN
        DEALLOCATE( grid%ts_filename )
      ENDIF 
      IF ( ASSOCIATED( grid%track_time_in ) ) THEN
        DEALLOCATE( grid%track_time_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lat_in ) ) THEN
        DEALLOCATE( grid%track_lat_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lon_in ) ) THEN
        DEALLOCATE( grid%track_lon_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_i ) ) THEN
        DEALLOCATE( grid%track_i )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_j ) ) THEN
        DEALLOCATE( grid%track_j )
      ENDIF

      IF ( ASSOCIATED( grid%track_time_domain ) ) THEN
        DEALLOCATE( grid%track_time_domain )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lat_domain ) ) THEN
        DEALLOCATE( grid%track_lat_domain )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lon_domain ) ) THEN
        DEALLOCATE( grid%track_lon_domain )
      ENDIF
      DEALLOCATE( grid )
      NULLIFY( grid )

   END SUBROUTINE domain_destroy

   SUBROUTINE dealloc_linked_lists ( grid )
      IMPLICIT NONE
      TYPE(domain), POINTER :: grid
      TYPE(fieldlist), POINTER :: p, q
      p => grid%head_statevars
      DO WHILE ( ASSOCIATED( p ) )
        if (p%varname.eq."chem_ic")  exit
         q => p ; p => p%next ; DEALLOCATE(q)
      ENDDO
      NULLIFY(grid%head_statevars) ; NULLIFY( grid%tail_statevars)
      IF ( .NOT. grid%is_intermediate ) THEN
        ALLOCATE( grid%head_statevars )
        NULLIFY( grid%head_statevars%next)
        grid%tail_statevars => grid%head_statevars
      ENDIF
   END SUBROUTINE dealloc_linked_lists

   RECURSIVE SUBROUTINE show_nest_subtree ( grid )
      TYPE(domain), POINTER :: grid
      INTEGER myid
      INTEGER kid
      IF ( .NOT. ASSOCIATED( grid ) ) RETURN
      myid = grid%id
      DO kid = 1, max_nests
        IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          IF ( grid%nests(kid)%ptr%id .EQ. myid ) THEN
            CALL wrf_error_fatal3("<stdin>",1601,&
'show_nest_subtree: nest hierarchy corrupted' )
          ENDIF
          CALL show_nest_subtree( grid%nests(kid)%ptr )
        ENDIF
      ENDDO
   END SUBROUTINE show_nest_subtree
   







   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain)              , POINTER :: grid

      

      INTEGER                             ::  ierr







IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1637,&
'frame/module_domain.f: Failed to deallocate grid%xlat. ')
 endif
  NULLIFY(grid%xlat)
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1645,&
'frame/module_domain.f: Failed to deallocate grid%xlong. ')
 endif
  NULLIFY(grid%xlong)
ENDIF
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1653,&
'frame/module_domain.f: Failed to deallocate grid%lu_index. ')
 endif
  NULLIFY(grid%lu_index)
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1661,&
'frame/module_domain.f: Failed to deallocate grid%lu_mask. ')
 endif
  NULLIFY(grid%lu_mask)
ENDIF
IF ( ASSOCIATED( grid%znu ) ) THEN 
  DEALLOCATE(grid%znu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1669,&
'frame/module_domain.f: Failed to deallocate grid%znu. ')
 endif
  NULLIFY(grid%znu)
ENDIF
IF ( ASSOCIATED( grid%znw ) ) THEN 
  DEALLOCATE(grid%znw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1677,&
'frame/module_domain.f: Failed to deallocate grid%znw. ')
 endif
  NULLIFY(grid%znw)
ENDIF
IF ( ASSOCIATED( grid%zs ) ) THEN 
  DEALLOCATE(grid%zs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1685,&
'frame/module_domain.f: Failed to deallocate grid%zs. ')
 endif
  NULLIFY(grid%zs)
ENDIF
IF ( ASSOCIATED( grid%dzs ) ) THEN 
  DEALLOCATE(grid%dzs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1693,&
'frame/module_domain.f: Failed to deallocate grid%dzs. ')
 endif
  NULLIFY(grid%dzs)
ENDIF
IF ( ASSOCIATED( grid%traj_i ) ) THEN 
  DEALLOCATE(grid%traj_i,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1701,&
'frame/module_domain.f: Failed to deallocate grid%traj_i. ')
 endif
  NULLIFY(grid%traj_i)
ENDIF
IF ( ASSOCIATED( grid%traj_j ) ) THEN 
  DEALLOCATE(grid%traj_j,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1709,&
'frame/module_domain.f: Failed to deallocate grid%traj_j. ')
 endif
  NULLIFY(grid%traj_j)
ENDIF
IF ( ASSOCIATED( grid%traj_k ) ) THEN 
  DEALLOCATE(grid%traj_k,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1717,&
'frame/module_domain.f: Failed to deallocate grid%traj_k. ')
 endif
  NULLIFY(grid%traj_k)
ENDIF
IF ( ASSOCIATED( grid%traj_long ) ) THEN 
  DEALLOCATE(grid%traj_long,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1725,&
'frame/module_domain.f: Failed to deallocate grid%traj_long. ')
 endif
  NULLIFY(grid%traj_long)
ENDIF
IF ( ASSOCIATED( grid%traj_lat ) ) THEN 
  DEALLOCATE(grid%traj_lat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1733,&
'frame/module_domain.f: Failed to deallocate grid%traj_lat. ')
 endif
  NULLIFY(grid%traj_lat)
ENDIF
IF ( ASSOCIATED( grid%u_gc ) ) THEN 
  DEALLOCATE(grid%u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1741,&
'frame/module_domain.f: Failed to deallocate grid%u_gc. ')
 endif
  NULLIFY(grid%u_gc)
ENDIF
IF ( ASSOCIATED( grid%v_gc ) ) THEN 
  DEALLOCATE(grid%v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1749,&
'frame/module_domain.f: Failed to deallocate grid%v_gc. ')
 endif
  NULLIFY(grid%v_gc)
ENDIF
IF ( ASSOCIATED( grid%t_gc ) ) THEN 
  DEALLOCATE(grid%t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1757,&
'frame/module_domain.f: Failed to deallocate grid%t_gc. ')
 endif
  NULLIFY(grid%t_gc)
ENDIF
IF ( ASSOCIATED( grid%rh_gc ) ) THEN 
  DEALLOCATE(grid%rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1765,&
'frame/module_domain.f: Failed to deallocate grid%rh_gc. ')
 endif
  NULLIFY(grid%rh_gc)
ENDIF
IF ( ASSOCIATED( grid%ght_gc ) ) THEN 
  DEALLOCATE(grid%ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1773,&
'frame/module_domain.f: Failed to deallocate grid%ght_gc. ')
 endif
  NULLIFY(grid%ght_gc)
ENDIF
IF ( ASSOCIATED( grid%p_gc ) ) THEN 
  DEALLOCATE(grid%p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1781,&
'frame/module_domain.f: Failed to deallocate grid%p_gc. ')
 endif
  NULLIFY(grid%p_gc)
ENDIF
IF ( ASSOCIATED( grid%prho_gc ) ) THEN 
  DEALLOCATE(grid%prho_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1789,&
'frame/module_domain.f: Failed to deallocate grid%prho_gc. ')
 endif
  NULLIFY(grid%prho_gc)
ENDIF
IF ( ASSOCIATED( grid%xlat_gc ) ) THEN 
  DEALLOCATE(grid%xlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1797,&
'frame/module_domain.f: Failed to deallocate grid%xlat_gc. ')
 endif
  NULLIFY(grid%xlat_gc)
ENDIF
IF ( ASSOCIATED( grid%xlong_gc ) ) THEN 
  DEALLOCATE(grid%xlong_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1805,&
'frame/module_domain.f: Failed to deallocate grid%xlong_gc. ')
 endif
  NULLIFY(grid%xlong_gc)
ENDIF
IF ( ASSOCIATED( grid%ht_gc ) ) THEN 
  DEALLOCATE(grid%ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1813,&
'frame/module_domain.f: Failed to deallocate grid%ht_gc. ')
 endif
  NULLIFY(grid%ht_gc)
ENDIF
IF ( ASSOCIATED( grid%var_sso ) ) THEN 
  DEALLOCATE(grid%var_sso,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1821,&
'frame/module_domain.f: Failed to deallocate grid%var_sso. ')
 endif
  NULLIFY(grid%var_sso)
ENDIF
IF ( ASSOCIATED( grid%lap_hgt ) ) THEN 
  DEALLOCATE(grid%lap_hgt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1829,&
'frame/module_domain.f: Failed to deallocate grid%lap_hgt. ')
 endif
  NULLIFY(grid%lap_hgt)
ENDIF
IF ( ASSOCIATED( grid%tsk_gc ) ) THEN 
  DEALLOCATE(grid%tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1837,&
'frame/module_domain.f: Failed to deallocate grid%tsk_gc. ')
 endif
  NULLIFY(grid%tsk_gc)
ENDIF
IF ( ASSOCIATED( grid%tavgsfc ) ) THEN 
  DEALLOCATE(grid%tavgsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1845,&
'frame/module_domain.f: Failed to deallocate grid%tavgsfc. ')
 endif
  NULLIFY(grid%tavgsfc)
ENDIF
IF ( ASSOCIATED( grid%tmn_gc ) ) THEN 
  DEALLOCATE(grid%tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1853,&
'frame/module_domain.f: Failed to deallocate grid%tmn_gc. ')
 endif
  NULLIFY(grid%tmn_gc)
ENDIF
IF ( ASSOCIATED( grid%pslv_gc ) ) THEN 
  DEALLOCATE(grid%pslv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1861,&
'frame/module_domain.f: Failed to deallocate grid%pslv_gc. ')
 endif
  NULLIFY(grid%pslv_gc)
ENDIF
IF ( ASSOCIATED( grid%sct_dom_gc ) ) THEN 
  DEALLOCATE(grid%sct_dom_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1869,&
'frame/module_domain.f: Failed to deallocate grid%sct_dom_gc. ')
 endif
  NULLIFY(grid%sct_dom_gc)
ENDIF
IF ( ASSOCIATED( grid%scb_dom_gc ) ) THEN 
  DEALLOCATE(grid%scb_dom_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1877,&
'frame/module_domain.f: Failed to deallocate grid%scb_dom_gc. ')
 endif
  NULLIFY(grid%scb_dom_gc)
ENDIF
IF ( ASSOCIATED( grid%greenfrac ) ) THEN 
  DEALLOCATE(grid%greenfrac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1885,&
'frame/module_domain.f: Failed to deallocate grid%greenfrac. ')
 endif
  NULLIFY(grid%greenfrac)
ENDIF
IF ( ASSOCIATED( grid%albedo12m ) ) THEN 
  DEALLOCATE(grid%albedo12m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1893,&
'frame/module_domain.f: Failed to deallocate grid%albedo12m. ')
 endif
  NULLIFY(grid%albedo12m)
ENDIF
IF ( ASSOCIATED( grid%lai12m ) ) THEN 
  DEALLOCATE(grid%lai12m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1901,&
'frame/module_domain.f: Failed to deallocate grid%lai12m. ')
 endif
  NULLIFY(grid%lai12m)
ENDIF
IF ( ASSOCIATED( grid%pd_gc ) ) THEN 
  DEALLOCATE(grid%pd_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1909,&
'frame/module_domain.f: Failed to deallocate grid%pd_gc. ')
 endif
  NULLIFY(grid%pd_gc)
ENDIF
IF ( ASSOCIATED( grid%pdrho_gc ) ) THEN 
  DEALLOCATE(grid%pdrho_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1917,&
'frame/module_domain.f: Failed to deallocate grid%pdrho_gc. ')
 endif
  NULLIFY(grid%pdrho_gc)
ENDIF
IF ( ASSOCIATED( grid%psfc_gc ) ) THEN 
  DEALLOCATE(grid%psfc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1925,&
'frame/module_domain.f: Failed to deallocate grid%psfc_gc. ')
 endif
  NULLIFY(grid%psfc_gc)
ENDIF
IF ( ASSOCIATED( grid%intq_gc ) ) THEN 
  DEALLOCATE(grid%intq_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1933,&
'frame/module_domain.f: Failed to deallocate grid%intq_gc. ')
 endif
  NULLIFY(grid%intq_gc)
ENDIF
IF ( ASSOCIATED( grid%pdhs ) ) THEN 
  DEALLOCATE(grid%pdhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1941,&
'frame/module_domain.f: Failed to deallocate grid%pdhs. ')
 endif
  NULLIFY(grid%pdhs)
ENDIF
IF ( ASSOCIATED( grid%qv_gc ) ) THEN 
  DEALLOCATE(grid%qv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1949,&
'frame/module_domain.f: Failed to deallocate grid%qv_gc. ')
 endif
  NULLIFY(grid%qv_gc)
ENDIF
IF ( ASSOCIATED( grid%sh_gc ) ) THEN 
  DEALLOCATE(grid%sh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1957,&
'frame/module_domain.f: Failed to deallocate grid%sh_gc. ')
 endif
  NULLIFY(grid%sh_gc)
ENDIF
IF ( ASSOCIATED( grid%cl_gc ) ) THEN 
  DEALLOCATE(grid%cl_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1965,&
'frame/module_domain.f: Failed to deallocate grid%cl_gc. ')
 endif
  NULLIFY(grid%cl_gc)
ENDIF
IF ( ASSOCIATED( grid%cf_gc ) ) THEN 
  DEALLOCATE(grid%cf_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1973,&
'frame/module_domain.f: Failed to deallocate grid%cf_gc. ')
 endif
  NULLIFY(grid%cf_gc)
ENDIF
IF ( ASSOCIATED( grid%icefrac_gc ) ) THEN 
  DEALLOCATE(grid%icefrac_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1981,&
'frame/module_domain.f: Failed to deallocate grid%icefrac_gc. ')
 endif
  NULLIFY(grid%icefrac_gc)
ENDIF
IF ( ASSOCIATED( grid%icepct ) ) THEN 
  DEALLOCATE(grid%icepct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1989,&
'frame/module_domain.f: Failed to deallocate grid%icepct. ')
 endif
  NULLIFY(grid%icepct)
ENDIF
IF ( ASSOCIATED( grid%qr_gc ) ) THEN 
  DEALLOCATE(grid%qr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1997,&
'frame/module_domain.f: Failed to deallocate grid%qr_gc. ')
 endif
  NULLIFY(grid%qr_gc)
ENDIF
IF ( ASSOCIATED( grid%qc_gc ) ) THEN 
  DEALLOCATE(grid%qc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2005,&
'frame/module_domain.f: Failed to deallocate grid%qc_gc. ')
 endif
  NULLIFY(grid%qc_gc)
ENDIF
IF ( ASSOCIATED( grid%qs_gc ) ) THEN 
  DEALLOCATE(grid%qs_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2013,&
'frame/module_domain.f: Failed to deallocate grid%qs_gc. ')
 endif
  NULLIFY(grid%qs_gc)
ENDIF
IF ( ASSOCIATED( grid%qi_gc ) ) THEN 
  DEALLOCATE(grid%qi_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2021,&
'frame/module_domain.f: Failed to deallocate grid%qi_gc. ')
 endif
  NULLIFY(grid%qi_gc)
ENDIF
IF ( ASSOCIATED( grid%qg_gc ) ) THEN 
  DEALLOCATE(grid%qg_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2029,&
'frame/module_domain.f: Failed to deallocate grid%qg_gc. ')
 endif
  NULLIFY(grid%qg_gc)
ENDIF
IF ( ASSOCIATED( grid%qh_gc ) ) THEN 
  DEALLOCATE(grid%qh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2037,&
'frame/module_domain.f: Failed to deallocate grid%qh_gc. ')
 endif
  NULLIFY(grid%qh_gc)
ENDIF
IF ( ASSOCIATED( grid%qni_gc ) ) THEN 
  DEALLOCATE(grid%qni_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2045,&
'frame/module_domain.f: Failed to deallocate grid%qni_gc. ')
 endif
  NULLIFY(grid%qni_gc)
ENDIF
IF ( ASSOCIATED( grid%qnc_gc ) ) THEN 
  DEALLOCATE(grid%qnc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2053,&
'frame/module_domain.f: Failed to deallocate grid%qnc_gc. ')
 endif
  NULLIFY(grid%qnc_gc)
ENDIF
IF ( ASSOCIATED( grid%qnr_gc ) ) THEN 
  DEALLOCATE(grid%qnr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2061,&
'frame/module_domain.f: Failed to deallocate grid%qnr_gc. ')
 endif
  NULLIFY(grid%qnr_gc)
ENDIF
IF ( ASSOCIATED( grid%qns_gc ) ) THEN 
  DEALLOCATE(grid%qns_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2069,&
'frame/module_domain.f: Failed to deallocate grid%qns_gc. ')
 endif
  NULLIFY(grid%qns_gc)
ENDIF
IF ( ASSOCIATED( grid%qng_gc ) ) THEN 
  DEALLOCATE(grid%qng_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2077,&
'frame/module_domain.f: Failed to deallocate grid%qng_gc. ')
 endif
  NULLIFY(grid%qng_gc)
ENDIF
IF ( ASSOCIATED( grid%qnh_gc ) ) THEN 
  DEALLOCATE(grid%qnh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2085,&
'frame/module_domain.f: Failed to deallocate grid%qnh_gc. ')
 endif
  NULLIFY(grid%qnh_gc)
ENDIF
IF ( ASSOCIATED( grid%qntemp ) ) THEN 
  DEALLOCATE(grid%qntemp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2093,&
'frame/module_domain.f: Failed to deallocate grid%qntemp. ')
 endif
  NULLIFY(grid%qntemp)
ENDIF
IF ( ASSOCIATED( grid%qntemp2 ) ) THEN 
  DEALLOCATE(grid%qntemp2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2101,&
'frame/module_domain.f: Failed to deallocate grid%qntemp2. ')
 endif
  NULLIFY(grid%qntemp2)
ENDIF
IF ( ASSOCIATED( grid%t_max_p ) ) THEN 
  DEALLOCATE(grid%t_max_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2109,&
'frame/module_domain.f: Failed to deallocate grid%t_max_p. ')
 endif
  NULLIFY(grid%t_max_p)
ENDIF
IF ( ASSOCIATED( grid%ght_max_p ) ) THEN 
  DEALLOCATE(grid%ght_max_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2117,&
'frame/module_domain.f: Failed to deallocate grid%ght_max_p. ')
 endif
  NULLIFY(grid%ght_max_p)
ENDIF
IF ( ASSOCIATED( grid%max_p ) ) THEN 
  DEALLOCATE(grid%max_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2125,&
'frame/module_domain.f: Failed to deallocate grid%max_p. ')
 endif
  NULLIFY(grid%max_p)
ENDIF
IF ( ASSOCIATED( grid%t_min_p ) ) THEN 
  DEALLOCATE(grid%t_min_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2133,&
'frame/module_domain.f: Failed to deallocate grid%t_min_p. ')
 endif
  NULLIFY(grid%t_min_p)
ENDIF
IF ( ASSOCIATED( grid%ght_min_p ) ) THEN 
  DEALLOCATE(grid%ght_min_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2141,&
'frame/module_domain.f: Failed to deallocate grid%ght_min_p. ')
 endif
  NULLIFY(grid%ght_min_p)
ENDIF
IF ( ASSOCIATED( grid%min_p ) ) THEN 
  DEALLOCATE(grid%min_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2149,&
'frame/module_domain.f: Failed to deallocate grid%min_p. ')
 endif
  NULLIFY(grid%min_p)
ENDIF
IF ( ASSOCIATED( grid%hgtmaxw ) ) THEN 
  DEALLOCATE(grid%hgtmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2157,&
'frame/module_domain.f: Failed to deallocate grid%hgtmaxw. ')
 endif
  NULLIFY(grid%hgtmaxw)
ENDIF
IF ( ASSOCIATED( grid%hgttrop ) ) THEN 
  DEALLOCATE(grid%hgttrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2165,&
'frame/module_domain.f: Failed to deallocate grid%hgttrop. ')
 endif
  NULLIFY(grid%hgttrop)
ENDIF
IF ( ASSOCIATED( grid%pmaxw ) ) THEN 
  DEALLOCATE(grid%pmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2173,&
'frame/module_domain.f: Failed to deallocate grid%pmaxw. ')
 endif
  NULLIFY(grid%pmaxw)
ENDIF
IF ( ASSOCIATED( grid%pmaxwnn ) ) THEN 
  DEALLOCATE(grid%pmaxwnn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2181,&
'frame/module_domain.f: Failed to deallocate grid%pmaxwnn. ')
 endif
  NULLIFY(grid%pmaxwnn)
ENDIF
IF ( ASSOCIATED( grid%ptrop ) ) THEN 
  DEALLOCATE(grid%ptrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2189,&
'frame/module_domain.f: Failed to deallocate grid%ptrop. ')
 endif
  NULLIFY(grid%ptrop)
ENDIF
IF ( ASSOCIATED( grid%ptropnn ) ) THEN 
  DEALLOCATE(grid%ptropnn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2197,&
'frame/module_domain.f: Failed to deallocate grid%ptropnn. ')
 endif
  NULLIFY(grid%ptropnn)
ENDIF
IF ( ASSOCIATED( grid%tmaxw ) ) THEN 
  DEALLOCATE(grid%tmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2205,&
'frame/module_domain.f: Failed to deallocate grid%tmaxw. ')
 endif
  NULLIFY(grid%tmaxw)
ENDIF
IF ( ASSOCIATED( grid%ttrop ) ) THEN 
  DEALLOCATE(grid%ttrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2213,&
'frame/module_domain.f: Failed to deallocate grid%ttrop. ')
 endif
  NULLIFY(grid%ttrop)
ENDIF
IF ( ASSOCIATED( grid%umaxw ) ) THEN 
  DEALLOCATE(grid%umaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2221,&
'frame/module_domain.f: Failed to deallocate grid%umaxw. ')
 endif
  NULLIFY(grid%umaxw)
ENDIF
IF ( ASSOCIATED( grid%utrop ) ) THEN 
  DEALLOCATE(grid%utrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2229,&
'frame/module_domain.f: Failed to deallocate grid%utrop. ')
 endif
  NULLIFY(grid%utrop)
ENDIF
IF ( ASSOCIATED( grid%vmaxw ) ) THEN 
  DEALLOCATE(grid%vmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2237,&
'frame/module_domain.f: Failed to deallocate grid%vmaxw. ')
 endif
  NULLIFY(grid%vmaxw)
ENDIF
IF ( ASSOCIATED( grid%vtrop ) ) THEN 
  DEALLOCATE(grid%vtrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2245,&
'frame/module_domain.f: Failed to deallocate grid%vtrop. ')
 endif
  NULLIFY(grid%vtrop)
ENDIF
IF ( ASSOCIATED( grid%erod ) ) THEN 
  DEALLOCATE(grid%erod,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2253,&
'frame/module_domain.f: Failed to deallocate grid%erod. ')
 endif
  NULLIFY(grid%erod)
ENDIF
IF ( ASSOCIATED( grid%bathymetry ) ) THEN 
  DEALLOCATE(grid%bathymetry,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2261,&
'frame/module_domain.f: Failed to deallocate grid%bathymetry. ')
 endif
  NULLIFY(grid%bathymetry)
ENDIF
IF ( ASSOCIATED( grid%u_1 ) ) THEN 
  DEALLOCATE(grid%u_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2269,&
'frame/module_domain.f: Failed to deallocate grid%u_1. ')
 endif
  NULLIFY(grid%u_1)
ENDIF
IF ( ASSOCIATED( grid%u_2 ) ) THEN 
  DEALLOCATE(grid%u_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2277,&
'frame/module_domain.f: Failed to deallocate grid%u_2. ')
 endif
  NULLIFY(grid%u_2)
ENDIF
IF ( ASSOCIATED( grid%u_bxs ) ) THEN 
  DEALLOCATE(grid%u_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2285,&
'frame/module_domain.f: Failed to deallocate grid%u_bxs. ')
 endif
  NULLIFY(grid%u_bxs)
ENDIF
IF ( ASSOCIATED( grid%u_bxe ) ) THEN 
  DEALLOCATE(grid%u_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2293,&
'frame/module_domain.f: Failed to deallocate grid%u_bxe. ')
 endif
  NULLIFY(grid%u_bxe)
ENDIF
IF ( ASSOCIATED( grid%u_bys ) ) THEN 
  DEALLOCATE(grid%u_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2301,&
'frame/module_domain.f: Failed to deallocate grid%u_bys. ')
 endif
  NULLIFY(grid%u_bys)
ENDIF
IF ( ASSOCIATED( grid%u_bye ) ) THEN 
  DEALLOCATE(grid%u_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2309,&
'frame/module_domain.f: Failed to deallocate grid%u_bye. ')
 endif
  NULLIFY(grid%u_bye)
ENDIF
IF ( ASSOCIATED( grid%u_btxs ) ) THEN 
  DEALLOCATE(grid%u_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2317,&
'frame/module_domain.f: Failed to deallocate grid%u_btxs. ')
 endif
  NULLIFY(grid%u_btxs)
ENDIF
IF ( ASSOCIATED( grid%u_btxe ) ) THEN 
  DEALLOCATE(grid%u_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2325,&
'frame/module_domain.f: Failed to deallocate grid%u_btxe. ')
 endif
  NULLIFY(grid%u_btxe)
ENDIF
IF ( ASSOCIATED( grid%u_btys ) ) THEN 
  DEALLOCATE(grid%u_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2333,&
'frame/module_domain.f: Failed to deallocate grid%u_btys. ')
 endif
  NULLIFY(grid%u_btys)
ENDIF
IF ( ASSOCIATED( grid%u_btye ) ) THEN 
  DEALLOCATE(grid%u_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2341,&
'frame/module_domain.f: Failed to deallocate grid%u_btye. ')
 endif
  NULLIFY(grid%u_btye)
ENDIF
IF ( ASSOCIATED( grid%ru ) ) THEN 
  DEALLOCATE(grid%ru,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2349,&
'frame/module_domain.f: Failed to deallocate grid%ru. ')
 endif
  NULLIFY(grid%ru)
ENDIF
IF ( ASSOCIATED( grid%ru_m ) ) THEN 
  DEALLOCATE(grid%ru_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2357,&
'frame/module_domain.f: Failed to deallocate grid%ru_m. ')
 endif
  NULLIFY(grid%ru_m)
ENDIF
IF ( ASSOCIATED( grid%ru_tend ) ) THEN 
  DEALLOCATE(grid%ru_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2365,&
'frame/module_domain.f: Failed to deallocate grid%ru_tend. ')
 endif
  NULLIFY(grid%ru_tend)
ENDIF
IF ( ASSOCIATED( grid%u_save ) ) THEN 
  DEALLOCATE(grid%u_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2373,&
'frame/module_domain.f: Failed to deallocate grid%u_save. ')
 endif
  NULLIFY(grid%u_save)
ENDIF
IF ( ASSOCIATED( grid%z_force ) ) THEN 
  DEALLOCATE(grid%z_force,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2381,&
'frame/module_domain.f: Failed to deallocate grid%z_force. ')
 endif
  NULLIFY(grid%z_force)
ENDIF
IF ( ASSOCIATED( grid%z_force_tend ) ) THEN 
  DEALLOCATE(grid%z_force_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2389,&
'frame/module_domain.f: Failed to deallocate grid%z_force_tend. ')
 endif
  NULLIFY(grid%z_force_tend)
ENDIF
IF ( ASSOCIATED( grid%u_g ) ) THEN 
  DEALLOCATE(grid%u_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2397,&
'frame/module_domain.f: Failed to deallocate grid%u_g. ')
 endif
  NULLIFY(grid%u_g)
ENDIF
IF ( ASSOCIATED( grid%u_g_tend ) ) THEN 
  DEALLOCATE(grid%u_g_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2405,&
'frame/module_domain.f: Failed to deallocate grid%u_g_tend. ')
 endif
  NULLIFY(grid%u_g_tend)
ENDIF
IF ( ASSOCIATED( grid%v_1 ) ) THEN 
  DEALLOCATE(grid%v_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2413,&
'frame/module_domain.f: Failed to deallocate grid%v_1. ')
 endif
  NULLIFY(grid%v_1)
ENDIF
IF ( ASSOCIATED( grid%v_2 ) ) THEN 
  DEALLOCATE(grid%v_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2421,&
'frame/module_domain.f: Failed to deallocate grid%v_2. ')
 endif
  NULLIFY(grid%v_2)
ENDIF
IF ( ASSOCIATED( grid%v_bxs ) ) THEN 
  DEALLOCATE(grid%v_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2429,&
'frame/module_domain.f: Failed to deallocate grid%v_bxs. ')
 endif
  NULLIFY(grid%v_bxs)
ENDIF
IF ( ASSOCIATED( grid%v_bxe ) ) THEN 
  DEALLOCATE(grid%v_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2437,&
'frame/module_domain.f: Failed to deallocate grid%v_bxe. ')
 endif
  NULLIFY(grid%v_bxe)
ENDIF
IF ( ASSOCIATED( grid%v_bys ) ) THEN 
  DEALLOCATE(grid%v_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2445,&
'frame/module_domain.f: Failed to deallocate grid%v_bys. ')
 endif
  NULLIFY(grid%v_bys)
ENDIF
IF ( ASSOCIATED( grid%v_bye ) ) THEN 
  DEALLOCATE(grid%v_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2453,&
'frame/module_domain.f: Failed to deallocate grid%v_bye. ')
 endif
  NULLIFY(grid%v_bye)
ENDIF
IF ( ASSOCIATED( grid%v_btxs ) ) THEN 
  DEALLOCATE(grid%v_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2461,&
'frame/module_domain.f: Failed to deallocate grid%v_btxs. ')
 endif
  NULLIFY(grid%v_btxs)
ENDIF
IF ( ASSOCIATED( grid%v_btxe ) ) THEN 
  DEALLOCATE(grid%v_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2469,&
'frame/module_domain.f: Failed to deallocate grid%v_btxe. ')
 endif
  NULLIFY(grid%v_btxe)
ENDIF
IF ( ASSOCIATED( grid%v_btys ) ) THEN 
  DEALLOCATE(grid%v_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2477,&
'frame/module_domain.f: Failed to deallocate grid%v_btys. ')
 endif
  NULLIFY(grid%v_btys)
ENDIF
IF ( ASSOCIATED( grid%v_btye ) ) THEN 
  DEALLOCATE(grid%v_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2485,&
'frame/module_domain.f: Failed to deallocate grid%v_btye. ')
 endif
  NULLIFY(grid%v_btye)
ENDIF
IF ( ASSOCIATED( grid%rv ) ) THEN 
  DEALLOCATE(grid%rv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2493,&
'frame/module_domain.f: Failed to deallocate grid%rv. ')
 endif
  NULLIFY(grid%rv)
ENDIF
IF ( ASSOCIATED( grid%rv_m ) ) THEN 
  DEALLOCATE(grid%rv_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2501,&
'frame/module_domain.f: Failed to deallocate grid%rv_m. ')
 endif
  NULLIFY(grid%rv_m)
ENDIF
IF ( ASSOCIATED( grid%rv_tend ) ) THEN 
  DEALLOCATE(grid%rv_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2509,&
'frame/module_domain.f: Failed to deallocate grid%rv_tend. ')
 endif
  NULLIFY(grid%rv_tend)
ENDIF
IF ( ASSOCIATED( grid%v_save ) ) THEN 
  DEALLOCATE(grid%v_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2517,&
'frame/module_domain.f: Failed to deallocate grid%v_save. ')
 endif
  NULLIFY(grid%v_save)
ENDIF
IF ( ASSOCIATED( grid%v_g ) ) THEN 
  DEALLOCATE(grid%v_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2525,&
'frame/module_domain.f: Failed to deallocate grid%v_g. ')
 endif
  NULLIFY(grid%v_g)
ENDIF
IF ( ASSOCIATED( grid%v_g_tend ) ) THEN 
  DEALLOCATE(grid%v_g_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2533,&
'frame/module_domain.f: Failed to deallocate grid%v_g_tend. ')
 endif
  NULLIFY(grid%v_g_tend)
ENDIF
IF ( ASSOCIATED( grid%w_1 ) ) THEN 
  DEALLOCATE(grid%w_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2541,&
'frame/module_domain.f: Failed to deallocate grid%w_1. ')
 endif
  NULLIFY(grid%w_1)
ENDIF
IF ( ASSOCIATED( grid%w_2 ) ) THEN 
  DEALLOCATE(grid%w_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2549,&
'frame/module_domain.f: Failed to deallocate grid%w_2. ')
 endif
  NULLIFY(grid%w_2)
ENDIF
IF ( ASSOCIATED( grid%w_bxs ) ) THEN 
  DEALLOCATE(grid%w_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2557,&
'frame/module_domain.f: Failed to deallocate grid%w_bxs. ')
 endif
  NULLIFY(grid%w_bxs)
ENDIF
IF ( ASSOCIATED( grid%w_bxe ) ) THEN 
  DEALLOCATE(grid%w_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2565,&
'frame/module_domain.f: Failed to deallocate grid%w_bxe. ')
 endif
  NULLIFY(grid%w_bxe)
ENDIF
IF ( ASSOCIATED( grid%w_bys ) ) THEN 
  DEALLOCATE(grid%w_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2573,&
'frame/module_domain.f: Failed to deallocate grid%w_bys. ')
 endif
  NULLIFY(grid%w_bys)
ENDIF
IF ( ASSOCIATED( grid%w_bye ) ) THEN 
  DEALLOCATE(grid%w_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2581,&
'frame/module_domain.f: Failed to deallocate grid%w_bye. ')
 endif
  NULLIFY(grid%w_bye)
ENDIF
IF ( ASSOCIATED( grid%w_btxs ) ) THEN 
  DEALLOCATE(grid%w_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2589,&
'frame/module_domain.f: Failed to deallocate grid%w_btxs. ')
 endif
  NULLIFY(grid%w_btxs)
ENDIF
IF ( ASSOCIATED( grid%w_btxe ) ) THEN 
  DEALLOCATE(grid%w_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2597,&
'frame/module_domain.f: Failed to deallocate grid%w_btxe. ')
 endif
  NULLIFY(grid%w_btxe)
ENDIF
IF ( ASSOCIATED( grid%w_btys ) ) THEN 
  DEALLOCATE(grid%w_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2605,&
'frame/module_domain.f: Failed to deallocate grid%w_btys. ')
 endif
  NULLIFY(grid%w_btys)
ENDIF
IF ( ASSOCIATED( grid%w_btye ) ) THEN 
  DEALLOCATE(grid%w_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2613,&
'frame/module_domain.f: Failed to deallocate grid%w_btye. ')
 endif
  NULLIFY(grid%w_btye)
ENDIF
IF ( ASSOCIATED( grid%ww ) ) THEN 
  DEALLOCATE(grid%ww,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2621,&
'frame/module_domain.f: Failed to deallocate grid%ww. ')
 endif
  NULLIFY(grid%ww)
ENDIF
IF ( ASSOCIATED( grid%rw ) ) THEN 
  DEALLOCATE(grid%rw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2629,&
'frame/module_domain.f: Failed to deallocate grid%rw. ')
 endif
  NULLIFY(grid%rw)
ENDIF
IF ( ASSOCIATED( grid%ww_m ) ) THEN 
  DEALLOCATE(grid%ww_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2637,&
'frame/module_domain.f: Failed to deallocate grid%ww_m. ')
 endif
  NULLIFY(grid%ww_m)
ENDIF
IF ( ASSOCIATED( grid%w_subs ) ) THEN 
  DEALLOCATE(grid%w_subs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2645,&
'frame/module_domain.f: Failed to deallocate grid%w_subs. ')
 endif
  NULLIFY(grid%w_subs)
ENDIF
IF ( ASSOCIATED( grid%w_subs_tend ) ) THEN 
  DEALLOCATE(grid%w_subs_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2653,&
'frame/module_domain.f: Failed to deallocate grid%w_subs_tend. ')
 endif
  NULLIFY(grid%w_subs_tend)
ENDIF
IF ( ASSOCIATED( grid%ph_1 ) ) THEN 
  DEALLOCATE(grid%ph_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2661,&
'frame/module_domain.f: Failed to deallocate grid%ph_1. ')
 endif
  NULLIFY(grid%ph_1)
ENDIF
IF ( ASSOCIATED( grid%ph_2 ) ) THEN 
  DEALLOCATE(grid%ph_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2669,&
'frame/module_domain.f: Failed to deallocate grid%ph_2. ')
 endif
  NULLIFY(grid%ph_2)
ENDIF
IF ( ASSOCIATED( grid%ph_bxs ) ) THEN 
  DEALLOCATE(grid%ph_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2677,&
'frame/module_domain.f: Failed to deallocate grid%ph_bxs. ')
 endif
  NULLIFY(grid%ph_bxs)
ENDIF
IF ( ASSOCIATED( grid%ph_bxe ) ) THEN 
  DEALLOCATE(grid%ph_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2685,&
'frame/module_domain.f: Failed to deallocate grid%ph_bxe. ')
 endif
  NULLIFY(grid%ph_bxe)
ENDIF
IF ( ASSOCIATED( grid%ph_bys ) ) THEN 
  DEALLOCATE(grid%ph_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2693,&
'frame/module_domain.f: Failed to deallocate grid%ph_bys. ')
 endif
  NULLIFY(grid%ph_bys)
ENDIF
IF ( ASSOCIATED( grid%ph_bye ) ) THEN 
  DEALLOCATE(grid%ph_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2701,&
'frame/module_domain.f: Failed to deallocate grid%ph_bye. ')
 endif
  NULLIFY(grid%ph_bye)
ENDIF
IF ( ASSOCIATED( grid%ph_btxs ) ) THEN 
  DEALLOCATE(grid%ph_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2709,&
'frame/module_domain.f: Failed to deallocate grid%ph_btxs. ')
 endif
  NULLIFY(grid%ph_btxs)
ENDIF
IF ( ASSOCIATED( grid%ph_btxe ) ) THEN 
  DEALLOCATE(grid%ph_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2717,&
'frame/module_domain.f: Failed to deallocate grid%ph_btxe. ')
 endif
  NULLIFY(grid%ph_btxe)
ENDIF
IF ( ASSOCIATED( grid%ph_btys ) ) THEN 
  DEALLOCATE(grid%ph_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2725,&
'frame/module_domain.f: Failed to deallocate grid%ph_btys. ')
 endif
  NULLIFY(grid%ph_btys)
ENDIF
IF ( ASSOCIATED( grid%ph_btye ) ) THEN 
  DEALLOCATE(grid%ph_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2733,&
'frame/module_domain.f: Failed to deallocate grid%ph_btye. ')
 endif
  NULLIFY(grid%ph_btye)
ENDIF
IF ( ASSOCIATED( grid%phb ) ) THEN 
  DEALLOCATE(grid%phb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2741,&
'frame/module_domain.f: Failed to deallocate grid%phb. ')
 endif
  NULLIFY(grid%phb)
ENDIF
IF ( ASSOCIATED( grid%phb_fine ) ) THEN 
  DEALLOCATE(grid%phb_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2749,&
'frame/module_domain.f: Failed to deallocate grid%phb_fine. ')
 endif
  NULLIFY(grid%phb_fine)
ENDIF
IF ( ASSOCIATED( grid%ph0 ) ) THEN 
  DEALLOCATE(grid%ph0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2757,&
'frame/module_domain.f: Failed to deallocate grid%ph0. ')
 endif
  NULLIFY(grid%ph0)
ENDIF
IF ( ASSOCIATED( grid%php ) ) THEN 
  DEALLOCATE(grid%php,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2765,&
'frame/module_domain.f: Failed to deallocate grid%php. ')
 endif
  NULLIFY(grid%php)
ENDIF
IF ( ASSOCIATED( grid%th_phy_m_t0 ) ) THEN 
  DEALLOCATE(grid%th_phy_m_t0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2773,&
'frame/module_domain.f: Failed to deallocate grid%th_phy_m_t0. ')
 endif
  NULLIFY(grid%th_phy_m_t0)
ENDIF
IF ( ASSOCIATED( grid%t_1 ) ) THEN 
  DEALLOCATE(grid%t_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2781,&
'frame/module_domain.f: Failed to deallocate grid%t_1. ')
 endif
  NULLIFY(grid%t_1)
ENDIF
IF ( ASSOCIATED( grid%t_2 ) ) THEN 
  DEALLOCATE(grid%t_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2789,&
'frame/module_domain.f: Failed to deallocate grid%t_2. ')
 endif
  NULLIFY(grid%t_2)
ENDIF
IF ( ASSOCIATED( grid%t_bxs ) ) THEN 
  DEALLOCATE(grid%t_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2797,&
'frame/module_domain.f: Failed to deallocate grid%t_bxs. ')
 endif
  NULLIFY(grid%t_bxs)
ENDIF
IF ( ASSOCIATED( grid%t_bxe ) ) THEN 
  DEALLOCATE(grid%t_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2805,&
'frame/module_domain.f: Failed to deallocate grid%t_bxe. ')
 endif
  NULLIFY(grid%t_bxe)
ENDIF
IF ( ASSOCIATED( grid%t_bys ) ) THEN 
  DEALLOCATE(grid%t_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2813,&
'frame/module_domain.f: Failed to deallocate grid%t_bys. ')
 endif
  NULLIFY(grid%t_bys)
ENDIF
IF ( ASSOCIATED( grid%t_bye ) ) THEN 
  DEALLOCATE(grid%t_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2821,&
'frame/module_domain.f: Failed to deallocate grid%t_bye. ')
 endif
  NULLIFY(grid%t_bye)
ENDIF
IF ( ASSOCIATED( grid%t_btxs ) ) THEN 
  DEALLOCATE(grid%t_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2829,&
'frame/module_domain.f: Failed to deallocate grid%t_btxs. ')
 endif
  NULLIFY(grid%t_btxs)
ENDIF
IF ( ASSOCIATED( grid%t_btxe ) ) THEN 
  DEALLOCATE(grid%t_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2837,&
'frame/module_domain.f: Failed to deallocate grid%t_btxe. ')
 endif
  NULLIFY(grid%t_btxe)
ENDIF
IF ( ASSOCIATED( grid%t_btys ) ) THEN 
  DEALLOCATE(grid%t_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2845,&
'frame/module_domain.f: Failed to deallocate grid%t_btys. ')
 endif
  NULLIFY(grid%t_btys)
ENDIF
IF ( ASSOCIATED( grid%t_btye ) ) THEN 
  DEALLOCATE(grid%t_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2853,&
'frame/module_domain.f: Failed to deallocate grid%t_btye. ')
 endif
  NULLIFY(grid%t_btye)
ENDIF
IF ( ASSOCIATED( grid%t_init ) ) THEN 
  DEALLOCATE(grid%t_init,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2861,&
'frame/module_domain.f: Failed to deallocate grid%t_init. ')
 endif
  NULLIFY(grid%t_init)
ENDIF
IF ( ASSOCIATED( grid%t_save ) ) THEN 
  DEALLOCATE(grid%t_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2869,&
'frame/module_domain.f: Failed to deallocate grid%t_save. ')
 endif
  NULLIFY(grid%t_save)
ENDIF
IF ( ASSOCIATED( grid%th_upstream_x ) ) THEN 
  DEALLOCATE(grid%th_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2877,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_x. ')
 endif
  NULLIFY(grid%th_upstream_x)
ENDIF
IF ( ASSOCIATED( grid%th_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%th_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2885,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_x_tend. ')
 endif
  NULLIFY(grid%th_upstream_x_tend)
ENDIF
IF ( ASSOCIATED( grid%th_upstream_y ) ) THEN 
  DEALLOCATE(grid%th_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2893,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_y. ')
 endif
  NULLIFY(grid%th_upstream_y)
ENDIF
IF ( ASSOCIATED( grid%th_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%th_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2901,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_y_tend. ')
 endif
  NULLIFY(grid%th_upstream_y_tend)
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_x ) ) THEN 
  DEALLOCATE(grid%qv_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2909,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_x. ')
 endif
  NULLIFY(grid%qv_upstream_x)
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%qv_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2917,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_x_tend. ')
 endif
  NULLIFY(grid%qv_upstream_x_tend)
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_y ) ) THEN 
  DEALLOCATE(grid%qv_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2925,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_y. ')
 endif
  NULLIFY(grid%qv_upstream_y)
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%qv_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2933,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_y_tend. ')
 endif
  NULLIFY(grid%qv_upstream_y_tend)
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_x ) ) THEN 
  DEALLOCATE(grid%ql_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2941,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_x. ')
 endif
  NULLIFY(grid%ql_upstream_x)
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%ql_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2949,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_x_tend. ')
 endif
  NULLIFY(grid%ql_upstream_x_tend)
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_y ) ) THEN 
  DEALLOCATE(grid%ql_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2957,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_y. ')
 endif
  NULLIFY(grid%ql_upstream_y)
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%ql_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2965,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_y_tend. ')
 endif
  NULLIFY(grid%ql_upstream_y_tend)
ENDIF
IF ( ASSOCIATED( grid%u_upstream_x ) ) THEN 
  DEALLOCATE(grid%u_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2973,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_x. ')
 endif
  NULLIFY(grid%u_upstream_x)
ENDIF
IF ( ASSOCIATED( grid%u_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%u_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2981,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_x_tend. ')
 endif
  NULLIFY(grid%u_upstream_x_tend)
ENDIF
IF ( ASSOCIATED( grid%u_upstream_y ) ) THEN 
  DEALLOCATE(grid%u_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2989,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_y. ')
 endif
  NULLIFY(grid%u_upstream_y)
ENDIF
IF ( ASSOCIATED( grid%u_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%u_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2997,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_y_tend. ')
 endif
  NULLIFY(grid%u_upstream_y_tend)
ENDIF
IF ( ASSOCIATED( grid%v_upstream_x ) ) THEN 
  DEALLOCATE(grid%v_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3005,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_x. ')
 endif
  NULLIFY(grid%v_upstream_x)
ENDIF
IF ( ASSOCIATED( grid%v_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%v_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3013,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_x_tend. ')
 endif
  NULLIFY(grid%v_upstream_x_tend)
ENDIF
IF ( ASSOCIATED( grid%v_upstream_y ) ) THEN 
  DEALLOCATE(grid%v_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3021,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_y. ')
 endif
  NULLIFY(grid%v_upstream_y)
ENDIF
IF ( ASSOCIATED( grid%v_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%v_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3029,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_y_tend. ')
 endif
  NULLIFY(grid%v_upstream_y_tend)
ENDIF
IF ( ASSOCIATED( grid%th_t_tend ) ) THEN 
  DEALLOCATE(grid%th_t_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3037,&
'frame/module_domain.f: Failed to deallocate grid%th_t_tend. ')
 endif
  NULLIFY(grid%th_t_tend)
ENDIF
IF ( ASSOCIATED( grid%qv_t_tend ) ) THEN 
  DEALLOCATE(grid%qv_t_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3045,&
'frame/module_domain.f: Failed to deallocate grid%qv_t_tend. ')
 endif
  NULLIFY(grid%qv_t_tend)
ENDIF
IF ( ASSOCIATED( grid%th_largescale ) ) THEN 
  DEALLOCATE(grid%th_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3053,&
'frame/module_domain.f: Failed to deallocate grid%th_largescale. ')
 endif
  NULLIFY(grid%th_largescale)
ENDIF
IF ( ASSOCIATED( grid%th_largescale_tend ) ) THEN 
  DEALLOCATE(grid%th_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3061,&
'frame/module_domain.f: Failed to deallocate grid%th_largescale_tend. ')
 endif
  NULLIFY(grid%th_largescale_tend)
ENDIF
IF ( ASSOCIATED( grid%qv_largescale ) ) THEN 
  DEALLOCATE(grid%qv_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3069,&
'frame/module_domain.f: Failed to deallocate grid%qv_largescale. ')
 endif
  NULLIFY(grid%qv_largescale)
ENDIF
IF ( ASSOCIATED( grid%qv_largescale_tend ) ) THEN 
  DEALLOCATE(grid%qv_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3077,&
'frame/module_domain.f: Failed to deallocate grid%qv_largescale_tend. ')
 endif
  NULLIFY(grid%qv_largescale_tend)
ENDIF
IF ( ASSOCIATED( grid%ql_largescale ) ) THEN 
  DEALLOCATE(grid%ql_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3085,&
'frame/module_domain.f: Failed to deallocate grid%ql_largescale. ')
 endif
  NULLIFY(grid%ql_largescale)
ENDIF
IF ( ASSOCIATED( grid%ql_largescale_tend ) ) THEN 
  DEALLOCATE(grid%ql_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3093,&
'frame/module_domain.f: Failed to deallocate grid%ql_largescale_tend. ')
 endif
  NULLIFY(grid%ql_largescale_tend)
ENDIF
IF ( ASSOCIATED( grid%u_largescale ) ) THEN 
  DEALLOCATE(grid%u_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3101,&
'frame/module_domain.f: Failed to deallocate grid%u_largescale. ')
 endif
  NULLIFY(grid%u_largescale)
ENDIF
IF ( ASSOCIATED( grid%u_largescale_tend ) ) THEN 
  DEALLOCATE(grid%u_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3109,&
'frame/module_domain.f: Failed to deallocate grid%u_largescale_tend. ')
 endif
  NULLIFY(grid%u_largescale_tend)
ENDIF
IF ( ASSOCIATED( grid%v_largescale ) ) THEN 
  DEALLOCATE(grid%v_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3117,&
'frame/module_domain.f: Failed to deallocate grid%v_largescale. ')
 endif
  NULLIFY(grid%v_largescale)
ENDIF
IF ( ASSOCIATED( grid%v_largescale_tend ) ) THEN 
  DEALLOCATE(grid%v_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3125,&
'frame/module_domain.f: Failed to deallocate grid%v_largescale_tend. ')
 endif
  NULLIFY(grid%v_largescale_tend)
ENDIF
IF ( ASSOCIATED( grid%tau_largescale ) ) THEN 
  DEALLOCATE(grid%tau_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3133,&
'frame/module_domain.f: Failed to deallocate grid%tau_largescale. ')
 endif
  NULLIFY(grid%tau_largescale)
ENDIF
IF ( ASSOCIATED( grid%tau_largescale_tend ) ) THEN 
  DEALLOCATE(grid%tau_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3141,&
'frame/module_domain.f: Failed to deallocate grid%tau_largescale_tend. ')
 endif
  NULLIFY(grid%tau_largescale_tend)
ENDIF
IF ( ASSOCIATED( grid%tau_x ) ) THEN 
  DEALLOCATE(grid%tau_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3149,&
'frame/module_domain.f: Failed to deallocate grid%tau_x. ')
 endif
  NULLIFY(grid%tau_x)
ENDIF
IF ( ASSOCIATED( grid%tau_x_tend ) ) THEN 
  DEALLOCATE(grid%tau_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3157,&
'frame/module_domain.f: Failed to deallocate grid%tau_x_tend. ')
 endif
  NULLIFY(grid%tau_x_tend)
ENDIF
IF ( ASSOCIATED( grid%tau_y ) ) THEN 
  DEALLOCATE(grid%tau_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3165,&
'frame/module_domain.f: Failed to deallocate grid%tau_y. ')
 endif
  NULLIFY(grid%tau_y)
ENDIF
IF ( ASSOCIATED( grid%tau_y_tend ) ) THEN 
  DEALLOCATE(grid%tau_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3173,&
'frame/module_domain.f: Failed to deallocate grid%tau_y_tend. ')
 endif
  NULLIFY(grid%tau_y_tend)
ENDIF
IF ( ASSOCIATED( grid%t_soil_forcing_val ) ) THEN 
  DEALLOCATE(grid%t_soil_forcing_val,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3181,&
'frame/module_domain.f: Failed to deallocate grid%t_soil_forcing_val. ')
 endif
  NULLIFY(grid%t_soil_forcing_val)
ENDIF
IF ( ASSOCIATED( grid%t_soil_forcing_tend ) ) THEN 
  DEALLOCATE(grid%t_soil_forcing_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3189,&
'frame/module_domain.f: Failed to deallocate grid%t_soil_forcing_tend. ')
 endif
  NULLIFY(grid%t_soil_forcing_tend)
ENDIF
IF ( ASSOCIATED( grid%q_soil_forcing_val ) ) THEN 
  DEALLOCATE(grid%q_soil_forcing_val,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3197,&
'frame/module_domain.f: Failed to deallocate grid%q_soil_forcing_val. ')
 endif
  NULLIFY(grid%q_soil_forcing_val)
ENDIF
IF ( ASSOCIATED( grid%q_soil_forcing_tend ) ) THEN 
  DEALLOCATE(grid%q_soil_forcing_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3205,&
'frame/module_domain.f: Failed to deallocate grid%q_soil_forcing_tend. ')
 endif
  NULLIFY(grid%q_soil_forcing_tend)
ENDIF
IF ( ASSOCIATED( grid%tau_soil ) ) THEN 
  DEALLOCATE(grid%tau_soil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3213,&
'frame/module_domain.f: Failed to deallocate grid%tau_soil. ')
 endif
  NULLIFY(grid%tau_soil)
ENDIF
IF ( ASSOCIATED( grid%soil_depth_force ) ) THEN 
  DEALLOCATE(grid%soil_depth_force,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3221,&
'frame/module_domain.f: Failed to deallocate grid%soil_depth_force. ')
 endif
  NULLIFY(grid%soil_depth_force)
ENDIF
IF ( ASSOCIATED( grid%mu_1 ) ) THEN 
  DEALLOCATE(grid%mu_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3229,&
'frame/module_domain.f: Failed to deallocate grid%mu_1. ')
 endif
  NULLIFY(grid%mu_1)
ENDIF
IF ( ASSOCIATED( grid%mu_2 ) ) THEN 
  DEALLOCATE(grid%mu_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3237,&
'frame/module_domain.f: Failed to deallocate grid%mu_2. ')
 endif
  NULLIFY(grid%mu_2)
ENDIF
IF ( ASSOCIATED( grid%mu_bxs ) ) THEN 
  DEALLOCATE(grid%mu_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3245,&
'frame/module_domain.f: Failed to deallocate grid%mu_bxs. ')
 endif
  NULLIFY(grid%mu_bxs)
ENDIF
IF ( ASSOCIATED( grid%mu_bxe ) ) THEN 
  DEALLOCATE(grid%mu_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3253,&
'frame/module_domain.f: Failed to deallocate grid%mu_bxe. ')
 endif
  NULLIFY(grid%mu_bxe)
ENDIF
IF ( ASSOCIATED( grid%mu_bys ) ) THEN 
  DEALLOCATE(grid%mu_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3261,&
'frame/module_domain.f: Failed to deallocate grid%mu_bys. ')
 endif
  NULLIFY(grid%mu_bys)
ENDIF
IF ( ASSOCIATED( grid%mu_bye ) ) THEN 
  DEALLOCATE(grid%mu_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3269,&
'frame/module_domain.f: Failed to deallocate grid%mu_bye. ')
 endif
  NULLIFY(grid%mu_bye)
ENDIF
IF ( ASSOCIATED( grid%mu_btxs ) ) THEN 
  DEALLOCATE(grid%mu_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3277,&
'frame/module_domain.f: Failed to deallocate grid%mu_btxs. ')
 endif
  NULLIFY(grid%mu_btxs)
ENDIF
IF ( ASSOCIATED( grid%mu_btxe ) ) THEN 
  DEALLOCATE(grid%mu_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3285,&
'frame/module_domain.f: Failed to deallocate grid%mu_btxe. ')
 endif
  NULLIFY(grid%mu_btxe)
ENDIF
IF ( ASSOCIATED( grid%mu_btys ) ) THEN 
  DEALLOCATE(grid%mu_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3293,&
'frame/module_domain.f: Failed to deallocate grid%mu_btys. ')
 endif
  NULLIFY(grid%mu_btys)
ENDIF
IF ( ASSOCIATED( grid%mu_btye ) ) THEN 
  DEALLOCATE(grid%mu_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3301,&
'frame/module_domain.f: Failed to deallocate grid%mu_btye. ')
 endif
  NULLIFY(grid%mu_btye)
ENDIF
IF ( ASSOCIATED( grid%mub ) ) THEN 
  DEALLOCATE(grid%mub,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3309,&
'frame/module_domain.f: Failed to deallocate grid%mub. ')
 endif
  NULLIFY(grid%mub)
ENDIF
IF ( ASSOCIATED( grid%mub_fine ) ) THEN 
  DEALLOCATE(grid%mub_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3317,&
'frame/module_domain.f: Failed to deallocate grid%mub_fine. ')
 endif
  NULLIFY(grid%mub_fine)
ENDIF
IF ( ASSOCIATED( grid%mub_save ) ) THEN 
  DEALLOCATE(grid%mub_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3325,&
'frame/module_domain.f: Failed to deallocate grid%mub_save. ')
 endif
  NULLIFY(grid%mub_save)
ENDIF
IF ( ASSOCIATED( grid%mu0 ) ) THEN 
  DEALLOCATE(grid%mu0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3333,&
'frame/module_domain.f: Failed to deallocate grid%mu0. ')
 endif
  NULLIFY(grid%mu0)
ENDIF
IF ( ASSOCIATED( grid%mudf ) ) THEN 
  DEALLOCATE(grid%mudf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3341,&
'frame/module_domain.f: Failed to deallocate grid%mudf. ')
 endif
  NULLIFY(grid%mudf)
ENDIF
IF ( ASSOCIATED( grid%muu ) ) THEN 
  DEALLOCATE(grid%muu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3349,&
'frame/module_domain.f: Failed to deallocate grid%muu. ')
 endif
  NULLIFY(grid%muu)
ENDIF
IF ( ASSOCIATED( grid%muus ) ) THEN 
  DEALLOCATE(grid%muus,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3357,&
'frame/module_domain.f: Failed to deallocate grid%muus. ')
 endif
  NULLIFY(grid%muus)
ENDIF
IF ( ASSOCIATED( grid%muv ) ) THEN 
  DEALLOCATE(grid%muv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3365,&
'frame/module_domain.f: Failed to deallocate grid%muv. ')
 endif
  NULLIFY(grid%muv)
ENDIF
IF ( ASSOCIATED( grid%muvs ) ) THEN 
  DEALLOCATE(grid%muvs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3373,&
'frame/module_domain.f: Failed to deallocate grid%muvs. ')
 endif
  NULLIFY(grid%muvs)
ENDIF
IF ( ASSOCIATED( grid%mut ) ) THEN 
  DEALLOCATE(grid%mut,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3381,&
'frame/module_domain.f: Failed to deallocate grid%mut. ')
 endif
  NULLIFY(grid%mut)
ENDIF
IF ( ASSOCIATED( grid%muts ) ) THEN 
  DEALLOCATE(grid%muts,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3389,&
'frame/module_domain.f: Failed to deallocate grid%muts. ')
 endif
  NULLIFY(grid%muts)
ENDIF
IF ( ASSOCIATED( grid%nest_pos ) ) THEN 
  DEALLOCATE(grid%nest_pos,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3397,&
'frame/module_domain.f: Failed to deallocate grid%nest_pos. ')
 endif
  NULLIFY(grid%nest_pos)
ENDIF
IF ( ASSOCIATED( grid%nest_mask ) ) THEN 
  DEALLOCATE(grid%nest_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3405,&
'frame/module_domain.f: Failed to deallocate grid%nest_mask. ')
 endif
  NULLIFY(grid%nest_mask)
ENDIF
IF ( ASSOCIATED( grid%ht_coarse ) ) THEN 
  DEALLOCATE(grid%ht_coarse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3413,&
'frame/module_domain.f: Failed to deallocate grid%ht_coarse. ')
 endif
  NULLIFY(grid%ht_coarse)
ENDIF
IF ( ASSOCIATED( grid%tke_1 ) ) THEN 
  DEALLOCATE(grid%tke_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3421,&
'frame/module_domain.f: Failed to deallocate grid%tke_1. ')
 endif
  NULLIFY(grid%tke_1)
ENDIF
IF ( ASSOCIATED( grid%tke_2 ) ) THEN 
  DEALLOCATE(grid%tke_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3429,&
'frame/module_domain.f: Failed to deallocate grid%tke_2. ')
 endif
  NULLIFY(grid%tke_2)
ENDIF
IF ( ASSOCIATED( grid%nlflux ) ) THEN 
  DEALLOCATE(grid%nlflux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3437,&
'frame/module_domain.f: Failed to deallocate grid%nlflux. ')
 endif
  NULLIFY(grid%nlflux)
ENDIF
IF ( ASSOCIATED( grid%gamu ) ) THEN 
  DEALLOCATE(grid%gamu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3445,&
'frame/module_domain.f: Failed to deallocate grid%gamu. ')
 endif
  NULLIFY(grid%gamu)
ENDIF
IF ( ASSOCIATED( grid%gamv ) ) THEN 
  DEALLOCATE(grid%gamv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3453,&
'frame/module_domain.f: Failed to deallocate grid%gamv. ')
 endif
  NULLIFY(grid%gamv)
ENDIF
IF ( ASSOCIATED( grid%dlk ) ) THEN 
  DEALLOCATE(grid%dlk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3461,&
'frame/module_domain.f: Failed to deallocate grid%dlk. ')
 endif
  NULLIFY(grid%dlk)
ENDIF
IF ( ASSOCIATED( grid%l_diss ) ) THEN 
  DEALLOCATE(grid%l_diss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3469,&
'frame/module_domain.f: Failed to deallocate grid%l_diss. ')
 endif
  NULLIFY(grid%l_diss)
ENDIF
IF ( ASSOCIATED( grid%elmin ) ) THEN 
  DEALLOCATE(grid%elmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3477,&
'frame/module_domain.f: Failed to deallocate grid%elmin. ')
 endif
  NULLIFY(grid%elmin)
ENDIF
IF ( ASSOCIATED( grid%xkmv_meso ) ) THEN 
  DEALLOCATE(grid%xkmv_meso,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3485,&
'frame/module_domain.f: Failed to deallocate grid%xkmv_meso. ')
 endif
  NULLIFY(grid%xkmv_meso)
ENDIF
IF ( ASSOCIATED( grid%p ) ) THEN 
  DEALLOCATE(grid%p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3493,&
'frame/module_domain.f: Failed to deallocate grid%p. ')
 endif
  NULLIFY(grid%p)
ENDIF
IF ( ASSOCIATED( grid%al ) ) THEN 
  DEALLOCATE(grid%al,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3501,&
'frame/module_domain.f: Failed to deallocate grid%al. ')
 endif
  NULLIFY(grid%al)
ENDIF
IF ( ASSOCIATED( grid%alt ) ) THEN 
  DEALLOCATE(grid%alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3509,&
'frame/module_domain.f: Failed to deallocate grid%alt. ')
 endif
  NULLIFY(grid%alt)
ENDIF
IF ( ASSOCIATED( grid%alb ) ) THEN 
  DEALLOCATE(grid%alb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3517,&
'frame/module_domain.f: Failed to deallocate grid%alb. ')
 endif
  NULLIFY(grid%alb)
ENDIF
IF ( ASSOCIATED( grid%zx ) ) THEN 
  DEALLOCATE(grid%zx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3525,&
'frame/module_domain.f: Failed to deallocate grid%zx. ')
 endif
  NULLIFY(grid%zx)
ENDIF
IF ( ASSOCIATED( grid%zy ) ) THEN 
  DEALLOCATE(grid%zy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3533,&
'frame/module_domain.f: Failed to deallocate grid%zy. ')
 endif
  NULLIFY(grid%zy)
ENDIF
IF ( ASSOCIATED( grid%rdz ) ) THEN 
  DEALLOCATE(grid%rdz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3541,&
'frame/module_domain.f: Failed to deallocate grid%rdz. ')
 endif
  NULLIFY(grid%rdz)
ENDIF
IF ( ASSOCIATED( grid%rdzw ) ) THEN 
  DEALLOCATE(grid%rdzw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3549,&
'frame/module_domain.f: Failed to deallocate grid%rdzw. ')
 endif
  NULLIFY(grid%rdzw)
ENDIF
IF ( ASSOCIATED( grid%pb ) ) THEN 
  DEALLOCATE(grid%pb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3557,&
'frame/module_domain.f: Failed to deallocate grid%pb. ')
 endif
  NULLIFY(grid%pb)
ENDIF
IF ( ASSOCIATED( grid%rho ) ) THEN 
  DEALLOCATE(grid%rho,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3565,&
'frame/module_domain.f: Failed to deallocate grid%rho. ')
 endif
  NULLIFY(grid%rho)
ENDIF
IF ( ASSOCIATED( grid%fnm ) ) THEN 
  DEALLOCATE(grid%fnm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3573,&
'frame/module_domain.f: Failed to deallocate grid%fnm. ')
 endif
  NULLIFY(grid%fnm)
ENDIF
IF ( ASSOCIATED( grid%fnp ) ) THEN 
  DEALLOCATE(grid%fnp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3581,&
'frame/module_domain.f: Failed to deallocate grid%fnp. ')
 endif
  NULLIFY(grid%fnp)
ENDIF
IF ( ASSOCIATED( grid%rdnw ) ) THEN 
  DEALLOCATE(grid%rdnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3589,&
'frame/module_domain.f: Failed to deallocate grid%rdnw. ')
 endif
  NULLIFY(grid%rdnw)
ENDIF
IF ( ASSOCIATED( grid%rdn ) ) THEN 
  DEALLOCATE(grid%rdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3597,&
'frame/module_domain.f: Failed to deallocate grid%rdn. ')
 endif
  NULLIFY(grid%rdn)
ENDIF
IF ( ASSOCIATED( grid%dnw ) ) THEN 
  DEALLOCATE(grid%dnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3605,&
'frame/module_domain.f: Failed to deallocate grid%dnw. ')
 endif
  NULLIFY(grid%dnw)
ENDIF
IF ( ASSOCIATED( grid%dn ) ) THEN 
  DEALLOCATE(grid%dn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3613,&
'frame/module_domain.f: Failed to deallocate grid%dn. ')
 endif
  NULLIFY(grid%dn)
ENDIF
IF ( ASSOCIATED( grid%t_base ) ) THEN 
  DEALLOCATE(grid%t_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3621,&
'frame/module_domain.f: Failed to deallocate grid%t_base. ')
 endif
  NULLIFY(grid%t_base)
ENDIF
IF ( ASSOCIATED( grid%z ) ) THEN 
  DEALLOCATE(grid%z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3629,&
'frame/module_domain.f: Failed to deallocate grid%z. ')
 endif
  NULLIFY(grid%z)
ENDIF
IF ( ASSOCIATED( grid%z_at_w ) ) THEN 
  DEALLOCATE(grid%z_at_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3637,&
'frame/module_domain.f: Failed to deallocate grid%z_at_w. ')
 endif
  NULLIFY(grid%z_at_w)
ENDIF
IF ( ASSOCIATED( grid%p_hyd ) ) THEN 
  DEALLOCATE(grid%p_hyd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3645,&
'frame/module_domain.f: Failed to deallocate grid%p_hyd. ')
 endif
  NULLIFY(grid%p_hyd)
ENDIF
IF ( ASSOCIATED( grid%p_hyd_w ) ) THEN 
  DEALLOCATE(grid%p_hyd_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3653,&
'frame/module_domain.f: Failed to deallocate grid%p_hyd_w. ')
 endif
  NULLIFY(grid%p_hyd_w)
ENDIF
IF ( ASSOCIATED( grid%q2 ) ) THEN 
  DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3661,&
'frame/module_domain.f: Failed to deallocate grid%q2. ')
 endif
  NULLIFY(grid%q2)
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3669,&
'frame/module_domain.f: Failed to deallocate grid%t2. ')
 endif
  NULLIFY(grid%t2)
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3677,&
'frame/module_domain.f: Failed to deallocate grid%th2. ')
 endif
  NULLIFY(grid%th2)
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3685,&
'frame/module_domain.f: Failed to deallocate grid%psfc. ')
 endif
  NULLIFY(grid%psfc)
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3693,&
'frame/module_domain.f: Failed to deallocate grid%u10. ')
 endif
  NULLIFY(grid%u10)
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3701,&
'frame/module_domain.f: Failed to deallocate grid%v10. ')
 endif
  NULLIFY(grid%v10)
ENDIF
IF ( ASSOCIATED( grid%lpi ) ) THEN 
  DEALLOCATE(grid%lpi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3709,&
'frame/module_domain.f: Failed to deallocate grid%lpi. ')
 endif
  NULLIFY(grid%lpi)
ENDIF
IF ( ASSOCIATED( grid%uratx ) ) THEN 
  DEALLOCATE(grid%uratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3717,&
'frame/module_domain.f: Failed to deallocate grid%uratx. ')
 endif
  NULLIFY(grid%uratx)
ENDIF
IF ( ASSOCIATED( grid%vratx ) ) THEN 
  DEALLOCATE(grid%vratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3725,&
'frame/module_domain.f: Failed to deallocate grid%vratx. ')
 endif
  NULLIFY(grid%vratx)
ENDIF
IF ( ASSOCIATED( grid%tratx ) ) THEN 
  DEALLOCATE(grid%tratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3733,&
'frame/module_domain.f: Failed to deallocate grid%tratx. ')
 endif
  NULLIFY(grid%tratx)
ENDIF
IF ( ASSOCIATED( grid%obs_savwt ) ) THEN 
  DEALLOCATE(grid%obs_savwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3741,&
'frame/module_domain.f: Failed to deallocate grid%obs_savwt. ')
 endif
  NULLIFY(grid%obs_savwt)
ENDIF
IF ( ASSOCIATED( grid%area2d ) ) THEN 
  DEALLOCATE(grid%area2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3749,&
'frame/module_domain.f: Failed to deallocate grid%area2d. ')
 endif
  NULLIFY(grid%area2d)
ENDIF
IF ( ASSOCIATED( grid%dx2d ) ) THEN 
  DEALLOCATE(grid%dx2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3757,&
'frame/module_domain.f: Failed to deallocate grid%dx2d. ')
 endif
  NULLIFY(grid%dx2d)
ENDIF
IF ( ASSOCIATED( grid%power ) ) THEN 
  DEALLOCATE(grid%power,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3765,&
'frame/module_domain.f: Failed to deallocate grid%power. ')
 endif
  NULLIFY(grid%power)
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3773,&
'frame/module_domain.f: Failed to deallocate grid%imask_nostag. ')
 endif
  NULLIFY(grid%imask_nostag)
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3781,&
'frame/module_domain.f: Failed to deallocate grid%imask_xstag. ')
 endif
  NULLIFY(grid%imask_xstag)
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3789,&
'frame/module_domain.f: Failed to deallocate grid%imask_ystag. ')
 endif
  NULLIFY(grid%imask_ystag)
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3797,&
'frame/module_domain.f: Failed to deallocate grid%imask_xystag. ')
 endif
  NULLIFY(grid%imask_xystag)
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3805,&
'frame/module_domain.f: Failed to deallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%moist_bxs ) ) THEN 
  DEALLOCATE(grid%moist_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3813,&
'frame/module_domain.f: Failed to deallocate grid%moist_bxs. ')
 endif
  NULLIFY(grid%moist_bxs)
ENDIF
IF ( ASSOCIATED( grid%moist_bxe ) ) THEN 
  DEALLOCATE(grid%moist_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3821,&
'frame/module_domain.f: Failed to deallocate grid%moist_bxe. ')
 endif
  NULLIFY(grid%moist_bxe)
ENDIF
IF ( ASSOCIATED( grid%moist_bys ) ) THEN 
  DEALLOCATE(grid%moist_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3829,&
'frame/module_domain.f: Failed to deallocate grid%moist_bys. ')
 endif
  NULLIFY(grid%moist_bys)
ENDIF
IF ( ASSOCIATED( grid%moist_bye ) ) THEN 
  DEALLOCATE(grid%moist_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3837,&
'frame/module_domain.f: Failed to deallocate grid%moist_bye. ')
 endif
  NULLIFY(grid%moist_bye)
ENDIF
IF ( ASSOCIATED( grid%moist_btxs ) ) THEN 
  DEALLOCATE(grid%moist_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3845,&
'frame/module_domain.f: Failed to deallocate grid%moist_btxs. ')
 endif
  NULLIFY(grid%moist_btxs)
ENDIF
IF ( ASSOCIATED( grid%moist_btxe ) ) THEN 
  DEALLOCATE(grid%moist_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3853,&
'frame/module_domain.f: Failed to deallocate grid%moist_btxe. ')
 endif
  NULLIFY(grid%moist_btxe)
ENDIF
IF ( ASSOCIATED( grid%moist_btys ) ) THEN 
  DEALLOCATE(grid%moist_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3861,&
'frame/module_domain.f: Failed to deallocate grid%moist_btys. ')
 endif
  NULLIFY(grid%moist_btys)
ENDIF
IF ( ASSOCIATED( grid%moist_btye ) ) THEN 
  DEALLOCATE(grid%moist_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3869,&
'frame/module_domain.f: Failed to deallocate grid%moist_btye. ')
 endif
  NULLIFY(grid%moist_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist ) ) THEN 
  DEALLOCATE(grid%dfi_moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3877,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist. ')
 endif
  NULLIFY(grid%dfi_moist)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3885,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bxs. ')
 endif
  NULLIFY(grid%dfi_moist_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3893,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bxe. ')
 endif
  NULLIFY(grid%dfi_moist_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bys ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3901,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bys. ')
 endif
  NULLIFY(grid%dfi_moist_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bye ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3909,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bye. ')
 endif
  NULLIFY(grid%dfi_moist_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3917,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btxs. ')
 endif
  NULLIFY(grid%dfi_moist_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3925,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btxe. ')
 endif
  NULLIFY(grid%dfi_moist_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btys ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3933,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btys. ')
 endif
  NULLIFY(grid%dfi_moist_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btye ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3941,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btye. ')
 endif
  NULLIFY(grid%dfi_moist_btye)
ENDIF
IF ( ASSOCIATED( grid%qvold ) ) THEN 
  DEALLOCATE(grid%qvold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3949,&
'frame/module_domain.f: Failed to deallocate grid%qvold. ')
 endif
  NULLIFY(grid%qvold)
ENDIF
IF ( ASSOCIATED( grid%rimi ) ) THEN 
  DEALLOCATE(grid%rimi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3957,&
'frame/module_domain.f: Failed to deallocate grid%rimi. ')
 endif
  NULLIFY(grid%rimi)
ENDIF
IF ( ASSOCIATED( grid%qnwfa2d ) ) THEN 
  DEALLOCATE(grid%qnwfa2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3965,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa2d. ')
 endif
  NULLIFY(grid%qnwfa2d)
ENDIF
IF ( ASSOCIATED( grid%qnifa2d ) ) THEN 
  DEALLOCATE(grid%qnifa2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3973,&
'frame/module_domain.f: Failed to deallocate grid%qnifa2d. ')
 endif
  NULLIFY(grid%qnifa2d)
ENDIF
IF ( ASSOCIATED( grid%qnbca2d ) ) THEN 
  DEALLOCATE(grid%qnbca2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3981,&
'frame/module_domain.f: Failed to deallocate grid%qnbca2d. ')
 endif
  NULLIFY(grid%qnbca2d)
ENDIF
IF ( ASSOCIATED( grid%qnocbb2d ) ) THEN 
  DEALLOCATE(grid%qnocbb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3989,&
'frame/module_domain.f: Failed to deallocate grid%qnocbb2d. ')
 endif
  NULLIFY(grid%qnocbb2d)
ENDIF
IF ( ASSOCIATED( grid%qnbcbb2d ) ) THEN 
  DEALLOCATE(grid%qnbcbb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3997,&
'frame/module_domain.f: Failed to deallocate grid%qnbcbb2d. ')
 endif
  NULLIFY(grid%qnbcbb2d)
ENDIF
IF ( ASSOCIATED( grid%re_cloud ) ) THEN 
  DEALLOCATE(grid%re_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4005,&
'frame/module_domain.f: Failed to deallocate grid%re_cloud. ')
 endif
  NULLIFY(grid%re_cloud)
ENDIF
IF ( ASSOCIATED( grid%re_ice ) ) THEN 
  DEALLOCATE(grid%re_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4013,&
'frame/module_domain.f: Failed to deallocate grid%re_ice. ')
 endif
  NULLIFY(grid%re_ice)
ENDIF
IF ( ASSOCIATED( grid%re_snow ) ) THEN 
  DEALLOCATE(grid%re_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4021,&
'frame/module_domain.f: Failed to deallocate grid%re_snow. ')
 endif
  NULLIFY(grid%re_snow)
ENDIF
IF ( ASSOCIATED( grid%re_cloud_gsfc ) ) THEN 
  DEALLOCATE(grid%re_cloud_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4029,&
'frame/module_domain.f: Failed to deallocate grid%re_cloud_gsfc. ')
 endif
  NULLIFY(grid%re_cloud_gsfc)
ENDIF
IF ( ASSOCIATED( grid%re_rain_gsfc ) ) THEN 
  DEALLOCATE(grid%re_rain_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4037,&
'frame/module_domain.f: Failed to deallocate grid%re_rain_gsfc. ')
 endif
  NULLIFY(grid%re_rain_gsfc)
ENDIF
IF ( ASSOCIATED( grid%re_ice_gsfc ) ) THEN 
  DEALLOCATE(grid%re_ice_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4045,&
'frame/module_domain.f: Failed to deallocate grid%re_ice_gsfc. ')
 endif
  NULLIFY(grid%re_ice_gsfc)
ENDIF
IF ( ASSOCIATED( grid%re_snow_gsfc ) ) THEN 
  DEALLOCATE(grid%re_snow_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4053,&
'frame/module_domain.f: Failed to deallocate grid%re_snow_gsfc. ')
 endif
  NULLIFY(grid%re_snow_gsfc)
ENDIF
IF ( ASSOCIATED( grid%re_graupel_gsfc ) ) THEN 
  DEALLOCATE(grid%re_graupel_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4061,&
'frame/module_domain.f: Failed to deallocate grid%re_graupel_gsfc. ')
 endif
  NULLIFY(grid%re_graupel_gsfc)
ENDIF
IF ( ASSOCIATED( grid%re_hail_gsfc ) ) THEN 
  DEALLOCATE(grid%re_hail_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4069,&
'frame/module_domain.f: Failed to deallocate grid%re_hail_gsfc. ')
 endif
  NULLIFY(grid%re_hail_gsfc)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_cloud ) ) THEN 
  DEALLOCATE(grid%dfi_re_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4077,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_cloud. ')
 endif
  NULLIFY(grid%dfi_re_cloud)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_ice ) ) THEN 
  DEALLOCATE(grid%dfi_re_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4085,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_ice. ')
 endif
  NULLIFY(grid%dfi_re_ice)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_snow ) ) THEN 
  DEALLOCATE(grid%dfi_re_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4093,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_snow. ')
 endif
  NULLIFY(grid%dfi_re_snow)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_cloud_gsfc ) ) THEN 
  DEALLOCATE(grid%dfi_re_cloud_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4101,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_cloud_gsfc. ')
 endif
  NULLIFY(grid%dfi_re_cloud_gsfc)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_rain_gsfc ) ) THEN 
  DEALLOCATE(grid%dfi_re_rain_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4109,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_rain_gsfc. ')
 endif
  NULLIFY(grid%dfi_re_rain_gsfc)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_ice_gsfc ) ) THEN 
  DEALLOCATE(grid%dfi_re_ice_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4117,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_ice_gsfc. ')
 endif
  NULLIFY(grid%dfi_re_ice_gsfc)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_snow_gsfc ) ) THEN 
  DEALLOCATE(grid%dfi_re_snow_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4125,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_snow_gsfc. ')
 endif
  NULLIFY(grid%dfi_re_snow_gsfc)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_graupel_gsfc ) ) THEN 
  DEALLOCATE(grid%dfi_re_graupel_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4133,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_graupel_gsfc. ')
 endif
  NULLIFY(grid%dfi_re_graupel_gsfc)
ENDIF
IF ( ASSOCIATED( grid%dfi_re_hail_gsfc ) ) THEN 
  DEALLOCATE(grid%dfi_re_hail_gsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4141,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_hail_gsfc. ')
 endif
  NULLIFY(grid%dfi_re_hail_gsfc)
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4149,&
'frame/module_domain.f: Failed to deallocate grid%scalar. ')
 endif
  NULLIFY(grid%scalar)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxs ) ) THEN 
  DEALLOCATE(grid%scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4157,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bxs. ')
 endif
  NULLIFY(grid%scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxe ) ) THEN 
  DEALLOCATE(grid%scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4165,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bxe. ')
 endif
  NULLIFY(grid%scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_bys ) ) THEN 
  DEALLOCATE(grid%scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4173,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bys. ')
 endif
  NULLIFY(grid%scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%scalar_bye ) ) THEN 
  DEALLOCATE(grid%scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4181,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bye. ')
 endif
  NULLIFY(grid%scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxs ) ) THEN 
  DEALLOCATE(grid%scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4189,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btxs. ')
 endif
  NULLIFY(grid%scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxe ) ) THEN 
  DEALLOCATE(grid%scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4197,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btxe. ')
 endif
  NULLIFY(grid%scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_btys ) ) THEN 
  DEALLOCATE(grid%scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4205,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btys. ')
 endif
  NULLIFY(grid%scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%scalar_btye ) ) THEN 
  DEALLOCATE(grid%scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4213,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btye. ')
 endif
  NULLIFY(grid%scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar ) ) THEN 
  DEALLOCATE(grid%dfi_scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4221,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar. ')
 endif
  NULLIFY(grid%dfi_scalar)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4229,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bxs. ')
 endif
  NULLIFY(grid%dfi_scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4237,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bxe. ')
 endif
  NULLIFY(grid%dfi_scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4245,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bys. ')
 endif
  NULLIFY(grid%dfi_scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4253,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bye. ')
 endif
  NULLIFY(grid%dfi_scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4261,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btxs. ')
 endif
  NULLIFY(grid%dfi_scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4269,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btxe. ')
 endif
  NULLIFY(grid%dfi_scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4277,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btys. ')
 endif
  NULLIFY(grid%dfi_scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4285,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btye. ')
 endif
  NULLIFY(grid%dfi_scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%fcx ) ) THEN 
  DEALLOCATE(grid%fcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4293,&
'frame/module_domain.f: Failed to deallocate grid%fcx. ')
 endif
  NULLIFY(grid%fcx)
ENDIF
IF ( ASSOCIATED( grid%gcx ) ) THEN 
  DEALLOCATE(grid%gcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4301,&
'frame/module_domain.f: Failed to deallocate grid%gcx. ')
 endif
  NULLIFY(grid%gcx)
ENDIF
IF ( ASSOCIATED( grid%soil_layers ) ) THEN 
  DEALLOCATE(grid%soil_layers,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4309,&
'frame/module_domain.f: Failed to deallocate grid%soil_layers. ')
 endif
  NULLIFY(grid%soil_layers)
ENDIF
IF ( ASSOCIATED( grid%soil_levels ) ) THEN 
  DEALLOCATE(grid%soil_levels,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4317,&
'frame/module_domain.f: Failed to deallocate grid%soil_levels. ')
 endif
  NULLIFY(grid%soil_levels)
ENDIF
IF ( ASSOCIATED( grid%st ) ) THEN 
  DEALLOCATE(grid%st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4325,&
'frame/module_domain.f: Failed to deallocate grid%st. ')
 endif
  NULLIFY(grid%st)
ENDIF
IF ( ASSOCIATED( grid%sm ) ) THEN 
  DEALLOCATE(grid%sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4333,&
'frame/module_domain.f: Failed to deallocate grid%sm. ')
 endif
  NULLIFY(grid%sm)
ENDIF
IF ( ASSOCIATED( grid%sw ) ) THEN 
  DEALLOCATE(grid%sw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4341,&
'frame/module_domain.f: Failed to deallocate grid%sw. ')
 endif
  NULLIFY(grid%sw)
ENDIF
IF ( ASSOCIATED( grid%soilt ) ) THEN 
  DEALLOCATE(grid%soilt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4349,&
'frame/module_domain.f: Failed to deallocate grid%soilt. ')
 endif
  NULLIFY(grid%soilt)
ENDIF
IF ( ASSOCIATED( grid%soilm ) ) THEN 
  DEALLOCATE(grid%soilm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4357,&
'frame/module_domain.f: Failed to deallocate grid%soilm. ')
 endif
  NULLIFY(grid%soilm)
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4365,&
'frame/module_domain.f: Failed to deallocate grid%sm000007. ')
 endif
  NULLIFY(grid%sm000007)
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4373,&
'frame/module_domain.f: Failed to deallocate grid%sm007028. ')
 endif
  NULLIFY(grid%sm007028)
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4381,&
'frame/module_domain.f: Failed to deallocate grid%sm028100. ')
 endif
  NULLIFY(grid%sm028100)
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4389,&
'frame/module_domain.f: Failed to deallocate grid%sm100255. ')
 endif
  NULLIFY(grid%sm100255)
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4397,&
'frame/module_domain.f: Failed to deallocate grid%st000007. ')
 endif
  NULLIFY(grid%st000007)
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4405,&
'frame/module_domain.f: Failed to deallocate grid%st007028. ')
 endif
  NULLIFY(grid%st007028)
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4413,&
'frame/module_domain.f: Failed to deallocate grid%st028100. ')
 endif
  NULLIFY(grid%st028100)
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4421,&
'frame/module_domain.f: Failed to deallocate grid%st100255. ')
 endif
  NULLIFY(grid%st100255)
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4429,&
'frame/module_domain.f: Failed to deallocate grid%sm000010. ')
 endif
  NULLIFY(grid%sm000010)
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4437,&
'frame/module_domain.f: Failed to deallocate grid%sm010040. ')
 endif
  NULLIFY(grid%sm010040)
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4445,&
'frame/module_domain.f: Failed to deallocate grid%sm040100. ')
 endif
  NULLIFY(grid%sm040100)
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4453,&
'frame/module_domain.f: Failed to deallocate grid%sm100200. ')
 endif
  NULLIFY(grid%sm100200)
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4461,&
'frame/module_domain.f: Failed to deallocate grid%sm010200. ')
 endif
  NULLIFY(grid%sm010200)
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4469,&
'frame/module_domain.f: Failed to deallocate grid%soilm000. ')
 endif
  NULLIFY(grid%soilm000)
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4477,&
'frame/module_domain.f: Failed to deallocate grid%soilm005. ')
 endif
  NULLIFY(grid%soilm005)
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4485,&
'frame/module_domain.f: Failed to deallocate grid%soilm020. ')
 endif
  NULLIFY(grid%soilm020)
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4493,&
'frame/module_domain.f: Failed to deallocate grid%soilm040. ')
 endif
  NULLIFY(grid%soilm040)
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4501,&
'frame/module_domain.f: Failed to deallocate grid%soilm160. ')
 endif
  NULLIFY(grid%soilm160)
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4509,&
'frame/module_domain.f: Failed to deallocate grid%soilm300. ')
 endif
  NULLIFY(grid%soilm300)
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4517,&
'frame/module_domain.f: Failed to deallocate grid%sw000010. ')
 endif
  NULLIFY(grid%sw000010)
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4525,&
'frame/module_domain.f: Failed to deallocate grid%sw010040. ')
 endif
  NULLIFY(grid%sw010040)
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4533,&
'frame/module_domain.f: Failed to deallocate grid%sw040100. ')
 endif
  NULLIFY(grid%sw040100)
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4541,&
'frame/module_domain.f: Failed to deallocate grid%sw100200. ')
 endif
  NULLIFY(grid%sw100200)
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4549,&
'frame/module_domain.f: Failed to deallocate grid%sw010200. ')
 endif
  NULLIFY(grid%sw010200)
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4557,&
'frame/module_domain.f: Failed to deallocate grid%soilw000. ')
 endif
  NULLIFY(grid%soilw000)
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4565,&
'frame/module_domain.f: Failed to deallocate grid%soilw005. ')
 endif
  NULLIFY(grid%soilw005)
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4573,&
'frame/module_domain.f: Failed to deallocate grid%soilw020. ')
 endif
  NULLIFY(grid%soilw020)
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4581,&
'frame/module_domain.f: Failed to deallocate grid%soilw040. ')
 endif
  NULLIFY(grid%soilw040)
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4589,&
'frame/module_domain.f: Failed to deallocate grid%soilw160. ')
 endif
  NULLIFY(grid%soilw160)
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4597,&
'frame/module_domain.f: Failed to deallocate grid%soilw300. ')
 endif
  NULLIFY(grid%soilw300)
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4605,&
'frame/module_domain.f: Failed to deallocate grid%st000010. ')
 endif
  NULLIFY(grid%st000010)
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4613,&
'frame/module_domain.f: Failed to deallocate grid%st010040. ')
 endif
  NULLIFY(grid%st010040)
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4621,&
'frame/module_domain.f: Failed to deallocate grid%st040100. ')
 endif
  NULLIFY(grid%st040100)
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4629,&
'frame/module_domain.f: Failed to deallocate grid%st100200. ')
 endif
  NULLIFY(grid%st100200)
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4637,&
'frame/module_domain.f: Failed to deallocate grid%st010200. ')
 endif
  NULLIFY(grid%st010200)
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4645,&
'frame/module_domain.f: Failed to deallocate grid%soilt000. ')
 endif
  NULLIFY(grid%soilt000)
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4653,&
'frame/module_domain.f: Failed to deallocate grid%soilt005. ')
 endif
  NULLIFY(grid%soilt005)
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4661,&
'frame/module_domain.f: Failed to deallocate grid%soilt020. ')
 endif
  NULLIFY(grid%soilt020)
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4669,&
'frame/module_domain.f: Failed to deallocate grid%soilt040. ')
 endif
  NULLIFY(grid%soilt040)
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4677,&
'frame/module_domain.f: Failed to deallocate grid%soilt160. ')
 endif
  NULLIFY(grid%soilt160)
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4685,&
'frame/module_domain.f: Failed to deallocate grid%soilt300. ')
 endif
  NULLIFY(grid%soilt300)
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4693,&
'frame/module_domain.f: Failed to deallocate grid%topostdv. ')
 endif
  NULLIFY(grid%topostdv)
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4701,&
'frame/module_domain.f: Failed to deallocate grid%toposlpx. ')
 endif
  NULLIFY(grid%toposlpx)
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4709,&
'frame/module_domain.f: Failed to deallocate grid%toposlpy. ')
 endif
  NULLIFY(grid%toposlpy)
ENDIF
IF ( ASSOCIATED( grid%slope ) ) THEN 
  DEALLOCATE(grid%slope,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4717,&
'frame/module_domain.f: Failed to deallocate grid%slope. ')
 endif
  NULLIFY(grid%slope)
ENDIF
IF ( ASSOCIATED( grid%slp_azi ) ) THEN 
  DEALLOCATE(grid%slp_azi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4725,&
'frame/module_domain.f: Failed to deallocate grid%slp_azi. ')
 endif
  NULLIFY(grid%slp_azi)
ENDIF
IF ( ASSOCIATED( grid%shdmax ) ) THEN 
  DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4733,&
'frame/module_domain.f: Failed to deallocate grid%shdmax. ')
 endif
  NULLIFY(grid%shdmax)
ENDIF
IF ( ASSOCIATED( grid%shdmin ) ) THEN 
  DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4741,&
'frame/module_domain.f: Failed to deallocate grid%shdmin. ')
 endif
  NULLIFY(grid%shdmin)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4749,&
'frame/module_domain.f: Failed to deallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4757,&
'frame/module_domain.f: Failed to deallocate grid%toposoil. ')
 endif
  NULLIFY(grid%toposoil)
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4765,&
'frame/module_domain.f: Failed to deallocate grid%landusef. ')
 endif
  NULLIFY(grid%landusef)
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4773,&
'frame/module_domain.f: Failed to deallocate grid%soilctop. ')
 endif
  NULLIFY(grid%soilctop)
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4781,&
'frame/module_domain.f: Failed to deallocate grid%soilcbot. ')
 endif
  NULLIFY(grid%soilcbot)
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4789,&
'frame/module_domain.f: Failed to deallocate grid%soilcat. ')
 endif
  NULLIFY(grid%soilcat)
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4797,&
'frame/module_domain.f: Failed to deallocate grid%vegcat. ')
 endif
  NULLIFY(grid%vegcat)
ENDIF
IF ( ASSOCIATED( grid%pct_pft_input ) ) THEN 
  DEALLOCATE(grid%pct_pft_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4805,&
'frame/module_domain.f: Failed to deallocate grid%pct_pft_input. ')
 endif
  NULLIFY(grid%pct_pft_input)
ENDIF
IF ( ASSOCIATED( grid%irrigation ) ) THEN 
  DEALLOCATE(grid%irrigation,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4813,&
'frame/module_domain.f: Failed to deallocate grid%irrigation. ')
 endif
  NULLIFY(grid%irrigation)
ENDIF
IF ( ASSOCIATED( grid%irr_rand_field ) ) THEN 
  DEALLOCATE(grid%irr_rand_field,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4821,&
'frame/module_domain.f: Failed to deallocate grid%irr_rand_field. ')
 endif
  NULLIFY(grid%irr_rand_field)
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4829,&
'frame/module_domain.f: Failed to deallocate grid%tslb. ')
 endif
  NULLIFY(grid%tslb)
ENDIF
IF ( ASSOCIATED( grid%ts_hour ) ) THEN 
  DEALLOCATE(grid%ts_hour,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4837,&
'frame/module_domain.f: Failed to deallocate grid%ts_hour. ')
 endif
  NULLIFY(grid%ts_hour)
ENDIF
IF ( ASSOCIATED( grid%ts_u ) ) THEN 
  DEALLOCATE(grid%ts_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4845,&
'frame/module_domain.f: Failed to deallocate grid%ts_u. ')
 endif
  NULLIFY(grid%ts_u)
ENDIF
IF ( ASSOCIATED( grid%ts_v ) ) THEN 
  DEALLOCATE(grid%ts_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4853,&
'frame/module_domain.f: Failed to deallocate grid%ts_v. ')
 endif
  NULLIFY(grid%ts_v)
ENDIF
IF ( ASSOCIATED( grid%ts_q ) ) THEN 
  DEALLOCATE(grid%ts_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4861,&
'frame/module_domain.f: Failed to deallocate grid%ts_q. ')
 endif
  NULLIFY(grid%ts_q)
ENDIF
IF ( ASSOCIATED( grid%ts_t ) ) THEN 
  DEALLOCATE(grid%ts_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4869,&
'frame/module_domain.f: Failed to deallocate grid%ts_t. ')
 endif
  NULLIFY(grid%ts_t)
ENDIF
IF ( ASSOCIATED( grid%ts_psfc ) ) THEN 
  DEALLOCATE(grid%ts_psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4877,&
'frame/module_domain.f: Failed to deallocate grid%ts_psfc. ')
 endif
  NULLIFY(grid%ts_psfc)
ENDIF
IF ( ASSOCIATED( grid%ts_glw ) ) THEN 
  DEALLOCATE(grid%ts_glw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4885,&
'frame/module_domain.f: Failed to deallocate grid%ts_glw. ')
 endif
  NULLIFY(grid%ts_glw)
ENDIF
IF ( ASSOCIATED( grid%ts_gsw ) ) THEN 
  DEALLOCATE(grid%ts_gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4893,&
'frame/module_domain.f: Failed to deallocate grid%ts_gsw. ')
 endif
  NULLIFY(grid%ts_gsw)
ENDIF
IF ( ASSOCIATED( grid%ts_hfx ) ) THEN 
  DEALLOCATE(grid%ts_hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4901,&
'frame/module_domain.f: Failed to deallocate grid%ts_hfx. ')
 endif
  NULLIFY(grid%ts_hfx)
ENDIF
IF ( ASSOCIATED( grid%ts_lh ) ) THEN 
  DEALLOCATE(grid%ts_lh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4909,&
'frame/module_domain.f: Failed to deallocate grid%ts_lh. ')
 endif
  NULLIFY(grid%ts_lh)
ENDIF
IF ( ASSOCIATED( grid%ts_tsk ) ) THEN 
  DEALLOCATE(grid%ts_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4917,&
'frame/module_domain.f: Failed to deallocate grid%ts_tsk. ')
 endif
  NULLIFY(grid%ts_tsk)
ENDIF
IF ( ASSOCIATED( grid%ts_tslb ) ) THEN 
  DEALLOCATE(grid%ts_tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4925,&
'frame/module_domain.f: Failed to deallocate grid%ts_tslb. ')
 endif
  NULLIFY(grid%ts_tslb)
ENDIF
IF ( ASSOCIATED( grid%ts_clw ) ) THEN 
  DEALLOCATE(grid%ts_clw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4933,&
'frame/module_domain.f: Failed to deallocate grid%ts_clw. ')
 endif
  NULLIFY(grid%ts_clw)
ENDIF
IF ( ASSOCIATED( grid%ts_rainc ) ) THEN 
  DEALLOCATE(grid%ts_rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4941,&
'frame/module_domain.f: Failed to deallocate grid%ts_rainc. ')
 endif
  NULLIFY(grid%ts_rainc)
ENDIF
IF ( ASSOCIATED( grid%ts_rainnc ) ) THEN 
  DEALLOCATE(grid%ts_rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4949,&
'frame/module_domain.f: Failed to deallocate grid%ts_rainnc. ')
 endif
  NULLIFY(grid%ts_rainnc)
ENDIF
IF ( ASSOCIATED( grid%ts_u_profile ) ) THEN 
  DEALLOCATE(grid%ts_u_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4957,&
'frame/module_domain.f: Failed to deallocate grid%ts_u_profile. ')
 endif
  NULLIFY(grid%ts_u_profile)
ENDIF
IF ( ASSOCIATED( grid%ts_v_profile ) ) THEN 
  DEALLOCATE(grid%ts_v_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4965,&
'frame/module_domain.f: Failed to deallocate grid%ts_v_profile. ')
 endif
  NULLIFY(grid%ts_v_profile)
ENDIF
IF ( ASSOCIATED( grid%ts_w_profile ) ) THEN 
  DEALLOCATE(grid%ts_w_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4973,&
'frame/module_domain.f: Failed to deallocate grid%ts_w_profile. ')
 endif
  NULLIFY(grid%ts_w_profile)
ENDIF
IF ( ASSOCIATED( grid%ts_gph_profile ) ) THEN 
  DEALLOCATE(grid%ts_gph_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4981,&
'frame/module_domain.f: Failed to deallocate grid%ts_gph_profile. ')
 endif
  NULLIFY(grid%ts_gph_profile)
ENDIF
IF ( ASSOCIATED( grid%ts_th_profile ) ) THEN 
  DEALLOCATE(grid%ts_th_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4989,&
'frame/module_domain.f: Failed to deallocate grid%ts_th_profile. ')
 endif
  NULLIFY(grid%ts_th_profile)
ENDIF
IF ( ASSOCIATED( grid%ts_qv_profile ) ) THEN 
  DEALLOCATE(grid%ts_qv_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4997,&
'frame/module_domain.f: Failed to deallocate grid%ts_qv_profile. ')
 endif
  NULLIFY(grid%ts_qv_profile)
ENDIF
IF ( ASSOCIATED( grid%ts_p_profile ) ) THEN 
  DEALLOCATE(grid%ts_p_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5005,&
'frame/module_domain.f: Failed to deallocate grid%ts_p_profile. ')
 endif
  NULLIFY(grid%ts_p_profile)
ENDIF
IF ( ASSOCIATED( grid%dzr ) ) THEN 
  DEALLOCATE(grid%dzr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5013,&
'frame/module_domain.f: Failed to deallocate grid%dzr. ')
 endif
  NULLIFY(grid%dzr)
ENDIF
IF ( ASSOCIATED( grid%dzb ) ) THEN 
  DEALLOCATE(grid%dzb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5021,&
'frame/module_domain.f: Failed to deallocate grid%dzb. ')
 endif
  NULLIFY(grid%dzb)
ENDIF
IF ( ASSOCIATED( grid%dzg ) ) THEN 
  DEALLOCATE(grid%dzg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5029,&
'frame/module_domain.f: Failed to deallocate grid%dzg. ')
 endif
  NULLIFY(grid%dzg)
ENDIF
IF ( ASSOCIATED( grid%urb_param ) ) THEN 
  DEALLOCATE(grid%urb_param,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5037,&
'frame/module_domain.f: Failed to deallocate grid%urb_param. ')
 endif
  NULLIFY(grid%urb_param)
ENDIF
IF ( ASSOCIATED( grid%lp_urb2d ) ) THEN 
  DEALLOCATE(grid%lp_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5045,&
'frame/module_domain.f: Failed to deallocate grid%lp_urb2d. ')
 endif
  NULLIFY(grid%lp_urb2d)
ENDIF
IF ( ASSOCIATED( grid%hi_urb2d ) ) THEN 
  DEALLOCATE(grid%hi_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5053,&
'frame/module_domain.f: Failed to deallocate grid%hi_urb2d. ')
 endif
  NULLIFY(grid%hi_urb2d)
ENDIF
IF ( ASSOCIATED( grid%lb_urb2d ) ) THEN 
  DEALLOCATE(grid%lb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5061,&
'frame/module_domain.f: Failed to deallocate grid%lb_urb2d. ')
 endif
  NULLIFY(grid%lb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%hgt_urb2d ) ) THEN 
  DEALLOCATE(grid%hgt_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5069,&
'frame/module_domain.f: Failed to deallocate grid%hgt_urb2d. ')
 endif
  NULLIFY(grid%hgt_urb2d)
ENDIF
IF ( ASSOCIATED( grid%mh_urb2d ) ) THEN 
  DEALLOCATE(grid%mh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5077,&
'frame/module_domain.f: Failed to deallocate grid%mh_urb2d. ')
 endif
  NULLIFY(grid%mh_urb2d)
ENDIF
IF ( ASSOCIATED( grid%stdh_urb2d ) ) THEN 
  DEALLOCATE(grid%stdh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5085,&
'frame/module_domain.f: Failed to deallocate grid%stdh_urb2d. ')
 endif
  NULLIFY(grid%stdh_urb2d)
ENDIF
IF ( ASSOCIATED( grid%lf_urb2d ) ) THEN 
  DEALLOCATE(grid%lf_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5093,&
'frame/module_domain.f: Failed to deallocate grid%lf_urb2d. ')
 endif
  NULLIFY(grid%lf_urb2d)
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5101,&
'frame/module_domain.f: Failed to deallocate grid%smois. ')
 endif
  NULLIFY(grid%smois)
ENDIF
IF ( ASSOCIATED( grid%sh2o ) ) THEN 
  DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5109,&
'frame/module_domain.f: Failed to deallocate grid%sh2o. ')
 endif
  NULLIFY(grid%sh2o)
ENDIF
IF ( ASSOCIATED( grid%smcrel ) ) THEN 
  DEALLOCATE(grid%smcrel,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5117,&
'frame/module_domain.f: Failed to deallocate grid%smcrel. ')
 endif
  NULLIFY(grid%smcrel)
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5125,&
'frame/module_domain.f: Failed to deallocate grid%xice. ')
 endif
  NULLIFY(grid%xice)
ENDIF
IF ( ASSOCIATED( grid%icedepth ) ) THEN 
  DEALLOCATE(grid%icedepth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5133,&
'frame/module_domain.f: Failed to deallocate grid%icedepth. ')
 endif
  NULLIFY(grid%icedepth)
ENDIF
IF ( ASSOCIATED( grid%xicem ) ) THEN 
  DEALLOCATE(grid%xicem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5141,&
'frame/module_domain.f: Failed to deallocate grid%xicem. ')
 endif
  NULLIFY(grid%xicem)
ENDIF
IF ( ASSOCIATED( grid%albsi ) ) THEN 
  DEALLOCATE(grid%albsi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5149,&
'frame/module_domain.f: Failed to deallocate grid%albsi. ')
 endif
  NULLIFY(grid%albsi)
ENDIF
IF ( ASSOCIATED( grid%snowsi ) ) THEN 
  DEALLOCATE(grid%snowsi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5157,&
'frame/module_domain.f: Failed to deallocate grid%snowsi. ')
 endif
  NULLIFY(grid%snowsi)
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5165,&
'frame/module_domain.f: Failed to deallocate grid%smstav. ')
 endif
  NULLIFY(grid%smstav)
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5173,&
'frame/module_domain.f: Failed to deallocate grid%smstot. ')
 endif
  NULLIFY(grid%smstot)
ENDIF
IF ( ASSOCIATED( grid%soldrain ) ) THEN 
  DEALLOCATE(grid%soldrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5181,&
'frame/module_domain.f: Failed to deallocate grid%soldrain. ')
 endif
  NULLIFY(grid%soldrain)
ENDIF
IF ( ASSOCIATED( grid%sfcheadrt ) ) THEN 
  DEALLOCATE(grid%sfcheadrt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5189,&
'frame/module_domain.f: Failed to deallocate grid%sfcheadrt. ')
 endif
  NULLIFY(grid%sfcheadrt)
ENDIF
IF ( ASSOCIATED( grid%infxsrt ) ) THEN 
  DEALLOCATE(grid%infxsrt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5197,&
'frame/module_domain.f: Failed to deallocate grid%infxsrt. ')
 endif
  NULLIFY(grid%infxsrt)
ENDIF
IF ( ASSOCIATED( grid%qtiledrain ) ) THEN 
  DEALLOCATE(grid%qtiledrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5205,&
'frame/module_domain.f: Failed to deallocate grid%qtiledrain. ')
 endif
  NULLIFY(grid%qtiledrain)
ENDIF
IF ( ASSOCIATED( grid%zwatble2d ) ) THEN 
  DEALLOCATE(grid%zwatble2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5213,&
'frame/module_domain.f: Failed to deallocate grid%zwatble2d. ')
 endif
  NULLIFY(grid%zwatble2d)
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5221,&
'frame/module_domain.f: Failed to deallocate grid%sfcrunoff. ')
 endif
  NULLIFY(grid%sfcrunoff)
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5229,&
'frame/module_domain.f: Failed to deallocate grid%udrunoff. ')
 endif
  NULLIFY(grid%udrunoff)
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5237,&
'frame/module_domain.f: Failed to deallocate grid%ivgtyp. ')
 endif
  NULLIFY(grid%ivgtyp)
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5245,&
'frame/module_domain.f: Failed to deallocate grid%isltyp. ')
 endif
  NULLIFY(grid%isltyp)
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5253,&
'frame/module_domain.f: Failed to deallocate grid%vegfra. ')
 endif
  NULLIFY(grid%vegfra)
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5261,&
'frame/module_domain.f: Failed to deallocate grid%sfcevp. ')
 endif
  NULLIFY(grid%sfcevp)
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5269,&
'frame/module_domain.f: Failed to deallocate grid%grdflx. ')
 endif
  NULLIFY(grid%grdflx)
ENDIF
IF ( ASSOCIATED( grid%acgrdflx ) ) THEN 
  DEALLOCATE(grid%acgrdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5277,&
'frame/module_domain.f: Failed to deallocate grid%acgrdflx. ')
 endif
  NULLIFY(grid%acgrdflx)
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5285,&
'frame/module_domain.f: Failed to deallocate grid%sfcexc. ')
 endif
  NULLIFY(grid%sfcexc)
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5293,&
'frame/module_domain.f: Failed to deallocate grid%acsnow. ')
 endif
  NULLIFY(grid%acsnow)
ENDIF
IF ( ASSOCIATED( grid%acrunoff ) ) THEN 
  DEALLOCATE(grid%acrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5301,&
'frame/module_domain.f: Failed to deallocate grid%acrunoff. ')
 endif
  NULLIFY(grid%acrunoff)
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5309,&
'frame/module_domain.f: Failed to deallocate grid%acsnom. ')
 endif
  NULLIFY(grid%acsnom)
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5317,&
'frame/module_domain.f: Failed to deallocate grid%snow. ')
 endif
  NULLIFY(grid%snow)
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5325,&
'frame/module_domain.f: Failed to deallocate grid%snowh. ')
 endif
  NULLIFY(grid%snowh)
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5333,&
'frame/module_domain.f: Failed to deallocate grid%canwat. ')
 endif
  NULLIFY(grid%canwat)
ENDIF
IF ( ASSOCIATED( grid%xlaidyn ) ) THEN 
  DEALLOCATE(grid%xlaidyn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5341,&
'frame/module_domain.f: Failed to deallocate grid%xlaidyn. ')
 endif
  NULLIFY(grid%xlaidyn)
ENDIF
IF ( ASSOCIATED( grid%sstsk ) ) THEN 
  DEALLOCATE(grid%sstsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5349,&
'frame/module_domain.f: Failed to deallocate grid%sstsk. ')
 endif
  NULLIFY(grid%sstsk)
ENDIF
IF ( ASSOCIATED( grid%lake_depth ) ) THEN 
  DEALLOCATE(grid%lake_depth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5357,&
'frame/module_domain.f: Failed to deallocate grid%lake_depth. ')
 endif
  NULLIFY(grid%lake_depth)
ENDIF
IF ( ASSOCIATED( grid%water_depth ) ) THEN 
  DEALLOCATE(grid%water_depth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5365,&
'frame/module_domain.f: Failed to deallocate grid%water_depth. ')
 endif
  NULLIFY(grid%water_depth)
ENDIF
IF ( ASSOCIATED( grid%dtw ) ) THEN 
  DEALLOCATE(grid%dtw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5373,&
'frame/module_domain.f: Failed to deallocate grid%dtw. ')
 endif
  NULLIFY(grid%dtw)
ENDIF
IF ( ASSOCIATED( grid%uoce ) ) THEN 
  DEALLOCATE(grid%uoce,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5381,&
'frame/module_domain.f: Failed to deallocate grid%uoce. ')
 endif
  NULLIFY(grid%uoce)
ENDIF
IF ( ASSOCIATED( grid%voce ) ) THEN 
  DEALLOCATE(grid%voce,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5389,&
'frame/module_domain.f: Failed to deallocate grid%voce. ')
 endif
  NULLIFY(grid%voce)
ENDIF
IF ( ASSOCIATED( grid%hcoeff ) ) THEN 
  DEALLOCATE(grid%hcoeff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5397,&
'frame/module_domain.f: Failed to deallocate grid%hcoeff. ')
 endif
  NULLIFY(grid%hcoeff)
ENDIF
IF ( ASSOCIATED( grid%dfi_p ) ) THEN 
  DEALLOCATE(grid%dfi_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5405,&
'frame/module_domain.f: Failed to deallocate grid%dfi_p. ')
 endif
  NULLIFY(grid%dfi_p)
ENDIF
IF ( ASSOCIATED( grid%dfi_al ) ) THEN 
  DEALLOCATE(grid%dfi_al,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5413,&
'frame/module_domain.f: Failed to deallocate grid%dfi_al. ')
 endif
  NULLIFY(grid%dfi_al)
ENDIF
IF ( ASSOCIATED( grid%dfi_mu ) ) THEN 
  DEALLOCATE(grid%dfi_mu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5421,&
'frame/module_domain.f: Failed to deallocate grid%dfi_mu. ')
 endif
  NULLIFY(grid%dfi_mu)
ENDIF
IF ( ASSOCIATED( grid%dfi_phb ) ) THEN 
  DEALLOCATE(grid%dfi_phb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5429,&
'frame/module_domain.f: Failed to deallocate grid%dfi_phb. ')
 endif
  NULLIFY(grid%dfi_phb)
ENDIF
IF ( ASSOCIATED( grid%dfi_ph0 ) ) THEN 
  DEALLOCATE(grid%dfi_ph0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5437,&
'frame/module_domain.f: Failed to deallocate grid%dfi_ph0. ')
 endif
  NULLIFY(grid%dfi_ph0)
ENDIF
IF ( ASSOCIATED( grid%dfi_php ) ) THEN 
  DEALLOCATE(grid%dfi_php,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5445,&
'frame/module_domain.f: Failed to deallocate grid%dfi_php. ')
 endif
  NULLIFY(grid%dfi_php)
ENDIF
IF ( ASSOCIATED( grid%dfi_u ) ) THEN 
  DEALLOCATE(grid%dfi_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5453,&
'frame/module_domain.f: Failed to deallocate grid%dfi_u. ')
 endif
  NULLIFY(grid%dfi_u)
ENDIF
IF ( ASSOCIATED( grid%dfi_v ) ) THEN 
  DEALLOCATE(grid%dfi_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5461,&
'frame/module_domain.f: Failed to deallocate grid%dfi_v. ')
 endif
  NULLIFY(grid%dfi_v)
ENDIF
IF ( ASSOCIATED( grid%dfi_w ) ) THEN 
  DEALLOCATE(grid%dfi_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5469,&
'frame/module_domain.f: Failed to deallocate grid%dfi_w. ')
 endif
  NULLIFY(grid%dfi_w)
ENDIF
IF ( ASSOCIATED( grid%dfi_ww ) ) THEN 
  DEALLOCATE(grid%dfi_ww,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5477,&
'frame/module_domain.f: Failed to deallocate grid%dfi_ww. ')
 endif
  NULLIFY(grid%dfi_ww)
ENDIF
IF ( ASSOCIATED( grid%dfi_t ) ) THEN 
  DEALLOCATE(grid%dfi_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5485,&
'frame/module_domain.f: Failed to deallocate grid%dfi_t. ')
 endif
  NULLIFY(grid%dfi_t)
ENDIF
IF ( ASSOCIATED( grid%dfi_rh ) ) THEN 
  DEALLOCATE(grid%dfi_rh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5493,&
'frame/module_domain.f: Failed to deallocate grid%dfi_rh. ')
 endif
  NULLIFY(grid%dfi_rh)
ENDIF
IF ( ASSOCIATED( grid%dfi_ph ) ) THEN 
  DEALLOCATE(grid%dfi_ph,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5501,&
'frame/module_domain.f: Failed to deallocate grid%dfi_ph. ')
 endif
  NULLIFY(grid%dfi_ph)
ENDIF
IF ( ASSOCIATED( grid%dfi_pb ) ) THEN 
  DEALLOCATE(grid%dfi_pb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5509,&
'frame/module_domain.f: Failed to deallocate grid%dfi_pb. ')
 endif
  NULLIFY(grid%dfi_pb)
ENDIF
IF ( ASSOCIATED( grid%dfi_alt ) ) THEN 
  DEALLOCATE(grid%dfi_alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5517,&
'frame/module_domain.f: Failed to deallocate grid%dfi_alt. ')
 endif
  NULLIFY(grid%dfi_alt)
ENDIF
IF ( ASSOCIATED( grid%dfi_tke ) ) THEN 
  DEALLOCATE(grid%dfi_tke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5525,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tke. ')
 endif
  NULLIFY(grid%dfi_tke)
ENDIF
IF ( ASSOCIATED( grid%dfi_tten_rad ) ) THEN 
  DEALLOCATE(grid%dfi_tten_rad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5533,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tten_rad. ')
 endif
  NULLIFY(grid%dfi_tten_rad)
ENDIF
IF ( ASSOCIATED( grid%dfi_tslb ) ) THEN 
  DEALLOCATE(grid%dfi_tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5541,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tslb. ')
 endif
  NULLIFY(grid%dfi_tslb)
ENDIF
IF ( ASSOCIATED( grid%dfi_smois ) ) THEN 
  DEALLOCATE(grid%dfi_smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5549,&
'frame/module_domain.f: Failed to deallocate grid%dfi_smois. ')
 endif
  NULLIFY(grid%dfi_smois)
ENDIF
IF ( ASSOCIATED( grid%dfi_snow ) ) THEN 
  DEALLOCATE(grid%dfi_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5557,&
'frame/module_domain.f: Failed to deallocate grid%dfi_snow. ')
 endif
  NULLIFY(grid%dfi_snow)
ENDIF
IF ( ASSOCIATED( grid%dfi_snowh ) ) THEN 
  DEALLOCATE(grid%dfi_snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5565,&
'frame/module_domain.f: Failed to deallocate grid%dfi_snowh. ')
 endif
  NULLIFY(grid%dfi_snowh)
ENDIF
IF ( ASSOCIATED( grid%dfi_canwat ) ) THEN 
  DEALLOCATE(grid%dfi_canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5573,&
'frame/module_domain.f: Failed to deallocate grid%dfi_canwat. ')
 endif
  NULLIFY(grid%dfi_canwat)
ENDIF
IF ( ASSOCIATED( grid%dfi_smfr3d ) ) THEN 
  DEALLOCATE(grid%dfi_smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5581,&
'frame/module_domain.f: Failed to deallocate grid%dfi_smfr3d. ')
 endif
  NULLIFY(grid%dfi_smfr3d)
ENDIF
IF ( ASSOCIATED( grid%dfi_keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%dfi_keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5589,&
'frame/module_domain.f: Failed to deallocate grid%dfi_keepfr3dflag. ')
 endif
  NULLIFY(grid%dfi_keepfr3dflag)
ENDIF
IF ( ASSOCIATED( grid%tsk_rural ) ) THEN 
  DEALLOCATE(grid%tsk_rural,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5597,&
'frame/module_domain.f: Failed to deallocate grid%tsk_rural. ')
 endif
  NULLIFY(grid%tsk_rural)
ENDIF
IF ( ASSOCIATED( grid%tr_urb2d ) ) THEN 
  DEALLOCATE(grid%tr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5605,&
'frame/module_domain.f: Failed to deallocate grid%tr_urb2d. ')
 endif
  NULLIFY(grid%tr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tgr_urb2d ) ) THEN 
  DEALLOCATE(grid%tgr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5613,&
'frame/module_domain.f: Failed to deallocate grid%tgr_urb2d. ')
 endif
  NULLIFY(grid%tgr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d ) ) THEN 
  DEALLOCATE(grid%tb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5621,&
'frame/module_domain.f: Failed to deallocate grid%tb_urb2d. ')
 endif
  NULLIFY(grid%tb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d ) ) THEN 
  DEALLOCATE(grid%tg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5629,&
'frame/module_domain.f: Failed to deallocate grid%tg_urb2d. ')
 endif
  NULLIFY(grid%tg_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d ) ) THEN 
  DEALLOCATE(grid%tc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5637,&
'frame/module_domain.f: Failed to deallocate grid%tc_urb2d. ')
 endif
  NULLIFY(grid%tc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d ) ) THEN 
  DEALLOCATE(grid%qc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5645,&
'frame/module_domain.f: Failed to deallocate grid%qc_urb2d. ')
 endif
  NULLIFY(grid%qc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d ) ) THEN 
  DEALLOCATE(grid%uc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5653,&
'frame/module_domain.f: Failed to deallocate grid%uc_urb2d. ')
 endif
  NULLIFY(grid%uc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxr_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5661,&
'frame/module_domain.f: Failed to deallocate grid%xxxr_urb2d. ')
 endif
  NULLIFY(grid%xxxr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxb_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5669,&
'frame/module_domain.f: Failed to deallocate grid%xxxb_urb2d. ')
 endif
  NULLIFY(grid%xxxb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxg_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5677,&
'frame/module_domain.f: Failed to deallocate grid%xxxg_urb2d. ')
 endif
  NULLIFY(grid%xxxg_urb2d)
ENDIF
IF ( ASSOCIATED( grid%xxxc_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5685,&
'frame/module_domain.f: Failed to deallocate grid%xxxc_urb2d. ')
 endif
  NULLIFY(grid%xxxc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%cmcr_urb2d ) ) THEN 
  DEALLOCATE(grid%cmcr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5693,&
'frame/module_domain.f: Failed to deallocate grid%cmcr_urb2d. ')
 endif
  NULLIFY(grid%cmcr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%drelr_urb2d ) ) THEN 
  DEALLOCATE(grid%drelr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5701,&
'frame/module_domain.f: Failed to deallocate grid%drelr_urb2d. ')
 endif
  NULLIFY(grid%drelr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%drelb_urb2d ) ) THEN 
  DEALLOCATE(grid%drelb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5709,&
'frame/module_domain.f: Failed to deallocate grid%drelb_urb2d. ')
 endif
  NULLIFY(grid%drelb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%drelg_urb2d ) ) THEN 
  DEALLOCATE(grid%drelg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5717,&
'frame/module_domain.f: Failed to deallocate grid%drelg_urb2d. ')
 endif
  NULLIFY(grid%drelg_urb2d)
ENDIF
IF ( ASSOCIATED( grid%flxhumr_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5725,&
'frame/module_domain.f: Failed to deallocate grid%flxhumr_urb2d. ')
 endif
  NULLIFY(grid%flxhumr_urb2d)
ENDIF
IF ( ASSOCIATED( grid%flxhumb_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5733,&
'frame/module_domain.f: Failed to deallocate grid%flxhumb_urb2d. ')
 endif
  NULLIFY(grid%flxhumb_urb2d)
ENDIF
IF ( ASSOCIATED( grid%flxhumg_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5741,&
'frame/module_domain.f: Failed to deallocate grid%flxhumg_urb2d. ')
 endif
  NULLIFY(grid%flxhumg_urb2d)
ENDIF
IF ( ASSOCIATED( grid%tgrl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgrl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5749,&
'frame/module_domain.f: Failed to deallocate grid%tgrl_urb3d. ')
 endif
  NULLIFY(grid%tgrl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%smr_urb3d ) ) THEN 
  DEALLOCATE(grid%smr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5757,&
'frame/module_domain.f: Failed to deallocate grid%smr_urb3d. ')
 endif
  NULLIFY(grid%smr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d ) ) THEN 
  DEALLOCATE(grid%trl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5765,&
'frame/module_domain.f: Failed to deallocate grid%trl_urb3d. ')
 endif
  NULLIFY(grid%trl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5773,&
'frame/module_domain.f: Failed to deallocate grid%tbl_urb3d. ')
 endif
  NULLIFY(grid%tbl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5781,&
'frame/module_domain.f: Failed to deallocate grid%tgl_urb3d. ')
 endif
  NULLIFY(grid%tgl_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d ) ) THEN 
  DEALLOCATE(grid%sh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5789,&
'frame/module_domain.f: Failed to deallocate grid%sh_urb2d. ')
 endif
  NULLIFY(grid%sh_urb2d)
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d ) ) THEN 
  DEALLOCATE(grid%lh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5797,&
'frame/module_domain.f: Failed to deallocate grid%lh_urb2d. ')
 endif
  NULLIFY(grid%lh_urb2d)
ENDIF
IF ( ASSOCIATED( grid%g_urb2d ) ) THEN 
  DEALLOCATE(grid%g_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5805,&
'frame/module_domain.f: Failed to deallocate grid%g_urb2d. ')
 endif
  NULLIFY(grid%g_urb2d)
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d ) ) THEN 
  DEALLOCATE(grid%rn_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5813,&
'frame/module_domain.f: Failed to deallocate grid%rn_urb2d. ')
 endif
  NULLIFY(grid%rn_urb2d)
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d ) ) THEN 
  DEALLOCATE(grid%ts_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5821,&
'frame/module_domain.f: Failed to deallocate grid%ts_urb2d. ')
 endif
  NULLIFY(grid%ts_urb2d)
ENDIF
IF ( ASSOCIATED( grid%frc_urb2d ) ) THEN 
  DEALLOCATE(grid%frc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5829,&
'frame/module_domain.f: Failed to deallocate grid%frc_urb2d. ')
 endif
  NULLIFY(grid%frc_urb2d)
ENDIF
IF ( ASSOCIATED( grid%utype_urb2d ) ) THEN 
  DEALLOCATE(grid%utype_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5837,&
'frame/module_domain.f: Failed to deallocate grid%utype_urb2d. ')
 endif
  NULLIFY(grid%utype_urb2d)
ENDIF
IF ( ASSOCIATED( grid%trb_urb4d ) ) THEN 
  DEALLOCATE(grid%trb_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5845,&
'frame/module_domain.f: Failed to deallocate grid%trb_urb4d. ')
 endif
  NULLIFY(grid%trb_urb4d)
ENDIF
IF ( ASSOCIATED( grid%tw1_urb4d ) ) THEN 
  DEALLOCATE(grid%tw1_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5853,&
'frame/module_domain.f: Failed to deallocate grid%tw1_urb4d. ')
 endif
  NULLIFY(grid%tw1_urb4d)
ENDIF
IF ( ASSOCIATED( grid%tw2_urb4d ) ) THEN 
  DEALLOCATE(grid%tw2_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5861,&
'frame/module_domain.f: Failed to deallocate grid%tw2_urb4d. ')
 endif
  NULLIFY(grid%tw2_urb4d)
ENDIF
IF ( ASSOCIATED( grid%tgb_urb4d ) ) THEN 
  DEALLOCATE(grid%tgb_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5869,&
'frame/module_domain.f: Failed to deallocate grid%tgb_urb4d. ')
 endif
  NULLIFY(grid%tgb_urb4d)
ENDIF
IF ( ASSOCIATED( grid%tlev_urb3d ) ) THEN 
  DEALLOCATE(grid%tlev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5877,&
'frame/module_domain.f: Failed to deallocate grid%tlev_urb3d. ')
 endif
  NULLIFY(grid%tlev_urb3d)
ENDIF
IF ( ASSOCIATED( grid%qlev_urb3d ) ) THEN 
  DEALLOCATE(grid%qlev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5885,&
'frame/module_domain.f: Failed to deallocate grid%qlev_urb3d. ')
 endif
  NULLIFY(grid%qlev_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tw1lev_urb3d ) ) THEN 
  DEALLOCATE(grid%tw1lev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5893,&
'frame/module_domain.f: Failed to deallocate grid%tw1lev_urb3d. ')
 endif
  NULLIFY(grid%tw1lev_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tw2lev_urb3d ) ) THEN 
  DEALLOCATE(grid%tw2lev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5901,&
'frame/module_domain.f: Failed to deallocate grid%tw2lev_urb3d. ')
 endif
  NULLIFY(grid%tw2lev_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tglev_urb3d ) ) THEN 
  DEALLOCATE(grid%tglev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5909,&
'frame/module_domain.f: Failed to deallocate grid%tglev_urb3d. ')
 endif
  NULLIFY(grid%tglev_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tflev_urb3d ) ) THEN 
  DEALLOCATE(grid%tflev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5917,&
'frame/module_domain.f: Failed to deallocate grid%tflev_urb3d. ')
 endif
  NULLIFY(grid%tflev_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sf_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%sf_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5925,&
'frame/module_domain.f: Failed to deallocate grid%sf_ac_urb3d. ')
 endif
  NULLIFY(grid%sf_ac_urb3d)
ENDIF
IF ( ASSOCIATED( grid%lf_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%lf_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5933,&
'frame/module_domain.f: Failed to deallocate grid%lf_ac_urb3d. ')
 endif
  NULLIFY(grid%lf_ac_urb3d)
ENDIF
IF ( ASSOCIATED( grid%cm_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%cm_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5941,&
'frame/module_domain.f: Failed to deallocate grid%cm_ac_urb3d. ')
 endif
  NULLIFY(grid%cm_ac_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfvent_urb3d ) ) THEN 
  DEALLOCATE(grid%sfvent_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5949,&
'frame/module_domain.f: Failed to deallocate grid%sfvent_urb3d. ')
 endif
  NULLIFY(grid%sfvent_urb3d)
ENDIF
IF ( ASSOCIATED( grid%lfvent_urb3d ) ) THEN 
  DEALLOCATE(grid%lfvent_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5957,&
'frame/module_domain.f: Failed to deallocate grid%lfvent_urb3d. ')
 endif
  NULLIFY(grid%lfvent_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfwin1_urb3d ) ) THEN 
  DEALLOCATE(grid%sfwin1_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5965,&
'frame/module_domain.f: Failed to deallocate grid%sfwin1_urb3d. ')
 endif
  NULLIFY(grid%sfwin1_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfwin2_urb3d ) ) THEN 
  DEALLOCATE(grid%sfwin2_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5973,&
'frame/module_domain.f: Failed to deallocate grid%sfwin2_urb3d. ')
 endif
  NULLIFY(grid%sfwin2_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfw1_urb3d ) ) THEN 
  DEALLOCATE(grid%sfw1_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5981,&
'frame/module_domain.f: Failed to deallocate grid%sfw1_urb3d. ')
 endif
  NULLIFY(grid%sfw1_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfw2_urb3d ) ) THEN 
  DEALLOCATE(grid%sfw2_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5989,&
'frame/module_domain.f: Failed to deallocate grid%sfw2_urb3d. ')
 endif
  NULLIFY(grid%sfw2_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfr_urb3d ) ) THEN 
  DEALLOCATE(grid%sfr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5997,&
'frame/module_domain.f: Failed to deallocate grid%sfr_urb3d. ')
 endif
  NULLIFY(grid%sfr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfg_urb3d ) ) THEN 
  DEALLOCATE(grid%sfg_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6005,&
'frame/module_domain.f: Failed to deallocate grid%sfg_urb3d. ')
 endif
  NULLIFY(grid%sfg_urb3d)
ENDIF
IF ( ASSOCIATED( grid%ep_pv_urb3d ) ) THEN 
  DEALLOCATE(grid%ep_pv_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6013,&
'frame/module_domain.f: Failed to deallocate grid%ep_pv_urb3d. ')
 endif
  NULLIFY(grid%ep_pv_urb3d)
ENDIF
IF ( ASSOCIATED( grid%t_pv_urb3d ) ) THEN 
  DEALLOCATE(grid%t_pv_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6021,&
'frame/module_domain.f: Failed to deallocate grid%t_pv_urb3d. ')
 endif
  NULLIFY(grid%t_pv_urb3d)
ENDIF
IF ( ASSOCIATED( grid%trv_urb4d ) ) THEN 
  DEALLOCATE(grid%trv_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6029,&
'frame/module_domain.f: Failed to deallocate grid%trv_urb4d. ')
 endif
  NULLIFY(grid%trv_urb4d)
ENDIF
IF ( ASSOCIATED( grid%qr_urb4d ) ) THEN 
  DEALLOCATE(grid%qr_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6037,&
'frame/module_domain.f: Failed to deallocate grid%qr_urb4d. ')
 endif
  NULLIFY(grid%qr_urb4d)
ENDIF
IF ( ASSOCIATED( grid%qgr_urb3d ) ) THEN 
  DEALLOCATE(grid%qgr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6045,&
'frame/module_domain.f: Failed to deallocate grid%qgr_urb3d. ')
 endif
  NULLIFY(grid%qgr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%tgr_urb3d ) ) THEN 
  DEALLOCATE(grid%tgr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6053,&
'frame/module_domain.f: Failed to deallocate grid%tgr_urb3d. ')
 endif
  NULLIFY(grid%tgr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%drain_urb4d ) ) THEN 
  DEALLOCATE(grid%drain_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6061,&
'frame/module_domain.f: Failed to deallocate grid%drain_urb4d. ')
 endif
  NULLIFY(grid%drain_urb4d)
ENDIF
IF ( ASSOCIATED( grid%draingr_urb3d ) ) THEN 
  DEALLOCATE(grid%draingr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6069,&
'frame/module_domain.f: Failed to deallocate grid%draingr_urb3d. ')
 endif
  NULLIFY(grid%draingr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%sfrv_urb3d ) ) THEN 
  DEALLOCATE(grid%sfrv_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6077,&
'frame/module_domain.f: Failed to deallocate grid%sfrv_urb3d. ')
 endif
  NULLIFY(grid%sfrv_urb3d)
ENDIF
IF ( ASSOCIATED( grid%lfrv_urb3d ) ) THEN 
  DEALLOCATE(grid%lfrv_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6085,&
'frame/module_domain.f: Failed to deallocate grid%lfrv_urb3d. ')
 endif
  NULLIFY(grid%lfrv_urb3d)
ENDIF
IF ( ASSOCIATED( grid%dgr_urb3d ) ) THEN 
  DEALLOCATE(grid%dgr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6093,&
'frame/module_domain.f: Failed to deallocate grid%dgr_urb3d. ')
 endif
  NULLIFY(grid%dgr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%dg_urb3d ) ) THEN 
  DEALLOCATE(grid%dg_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6101,&
'frame/module_domain.f: Failed to deallocate grid%dg_urb3d. ')
 endif
  NULLIFY(grid%dg_urb3d)
ENDIF
IF ( ASSOCIATED( grid%lfr_urb3d ) ) THEN 
  DEALLOCATE(grid%lfr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6109,&
'frame/module_domain.f: Failed to deallocate grid%lfr_urb3d. ')
 endif
  NULLIFY(grid%lfr_urb3d)
ENDIF
IF ( ASSOCIATED( grid%lfg_urb3d ) ) THEN 
  DEALLOCATE(grid%lfg_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6117,&
'frame/module_domain.f: Failed to deallocate grid%lfg_urb3d. ')
 endif
  NULLIFY(grid%lfg_urb3d)
ENDIF
IF ( ASSOCIATED( grid%cmr_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6125,&
'frame/module_domain.f: Failed to deallocate grid%cmr_sfcdif. ')
 endif
  NULLIFY(grid%cmr_sfcdif)
ENDIF
IF ( ASSOCIATED( grid%chr_sfcdif ) ) THEN 
  DEALLOCATE(grid%chr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6133,&
'frame/module_domain.f: Failed to deallocate grid%chr_sfcdif. ')
 endif
  NULLIFY(grid%chr_sfcdif)
ENDIF
IF ( ASSOCIATED( grid%cmc_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmc_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6141,&
'frame/module_domain.f: Failed to deallocate grid%cmc_sfcdif. ')
 endif
  NULLIFY(grid%cmc_sfcdif)
ENDIF
IF ( ASSOCIATED( grid%chc_sfcdif ) ) THEN 
  DEALLOCATE(grid%chc_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6149,&
'frame/module_domain.f: Failed to deallocate grid%chc_sfcdif. ')
 endif
  NULLIFY(grid%chc_sfcdif)
ENDIF
IF ( ASSOCIATED( grid%cmgr_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmgr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6157,&
'frame/module_domain.f: Failed to deallocate grid%cmgr_sfcdif. ')
 endif
  NULLIFY(grid%cmgr_sfcdif)
ENDIF
IF ( ASSOCIATED( grid%chgr_sfcdif ) ) THEN 
  DEALLOCATE(grid%chgr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6165,&
'frame/module_domain.f: Failed to deallocate grid%chgr_sfcdif. ')
 endif
  NULLIFY(grid%chgr_sfcdif)
ENDIF
IF ( ASSOCIATED( grid%ecmask ) ) THEN 
  DEALLOCATE(grid%ecmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6173,&
'frame/module_domain.f: Failed to deallocate grid%ecmask. ')
 endif
  NULLIFY(grid%ecmask)
ENDIF
IF ( ASSOCIATED( grid%ecobsc ) ) THEN 
  DEALLOCATE(grid%ecobsc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6181,&
'frame/module_domain.f: Failed to deallocate grid%ecobsc. ')
 endif
  NULLIFY(grid%ecobsc)
ENDIF
IF ( ASSOCIATED( grid%coszen ) ) THEN 
  DEALLOCATE(grid%coszen,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6189,&
'frame/module_domain.f: Failed to deallocate grid%coszen. ')
 endif
  NULLIFY(grid%coszen)
ENDIF
IF ( ASSOCIATED( grid%hrang ) ) THEN 
  DEALLOCATE(grid%hrang,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6197,&
'frame/module_domain.f: Failed to deallocate grid%hrang. ')
 endif
  NULLIFY(grid%hrang)
ENDIF
IF ( ASSOCIATED( grid%rhosnf ) ) THEN 
  DEALLOCATE(grid%rhosnf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6205,&
'frame/module_domain.f: Failed to deallocate grid%rhosnf. ')
 endif
  NULLIFY(grid%rhosnf)
ENDIF
IF ( ASSOCIATED( grid%snowfallac ) ) THEN 
  DEALLOCATE(grid%snowfallac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6213,&
'frame/module_domain.f: Failed to deallocate grid%snowfallac. ')
 endif
  NULLIFY(grid%snowfallac)
ENDIF
IF ( ASSOCIATED( grid%precipfr ) ) THEN 
  DEALLOCATE(grid%precipfr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6221,&
'frame/module_domain.f: Failed to deallocate grid%precipfr. ')
 endif
  NULLIFY(grid%precipfr)
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6229,&
'frame/module_domain.f: Failed to deallocate grid%smfr3d. ')
 endif
  NULLIFY(grid%smfr3d)
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6237,&
'frame/module_domain.f: Failed to deallocate grid%keepfr3dflag. ')
 endif
  NULLIFY(grid%keepfr3dflag)
ENDIF
IF ( ASSOCIATED( grid%swvisdir ) ) THEN 
  DEALLOCATE(grid%swvisdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6245,&
'frame/module_domain.f: Failed to deallocate grid%swvisdir. ')
 endif
  NULLIFY(grid%swvisdir)
ENDIF
IF ( ASSOCIATED( grid%swvisdif ) ) THEN 
  DEALLOCATE(grid%swvisdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6253,&
'frame/module_domain.f: Failed to deallocate grid%swvisdif. ')
 endif
  NULLIFY(grid%swvisdif)
ENDIF
IF ( ASSOCIATED( grid%swnirdir ) ) THEN 
  DEALLOCATE(grid%swnirdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6261,&
'frame/module_domain.f: Failed to deallocate grid%swnirdir. ')
 endif
  NULLIFY(grid%swnirdir)
ENDIF
IF ( ASSOCIATED( grid%swnirdif ) ) THEN 
  DEALLOCATE(grid%swnirdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6269,&
'frame/module_domain.f: Failed to deallocate grid%swnirdif. ')
 endif
  NULLIFY(grid%swnirdif)
ENDIF
IF ( ASSOCIATED( grid%alswvisdir ) ) THEN 
  DEALLOCATE(grid%alswvisdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6277,&
'frame/module_domain.f: Failed to deallocate grid%alswvisdir. ')
 endif
  NULLIFY(grid%alswvisdir)
ENDIF
IF ( ASSOCIATED( grid%alswvisdif ) ) THEN 
  DEALLOCATE(grid%alswvisdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6285,&
'frame/module_domain.f: Failed to deallocate grid%alswvisdif. ')
 endif
  NULLIFY(grid%alswvisdif)
ENDIF
IF ( ASSOCIATED( grid%alswnirdir ) ) THEN 
  DEALLOCATE(grid%alswnirdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6293,&
'frame/module_domain.f: Failed to deallocate grid%alswnirdir. ')
 endif
  NULLIFY(grid%alswnirdir)
ENDIF
IF ( ASSOCIATED( grid%alswnirdif ) ) THEN 
  DEALLOCATE(grid%alswnirdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6301,&
'frame/module_domain.f: Failed to deallocate grid%alswnirdif. ')
 endif
  NULLIFY(grid%alswnirdif)
ENDIF
IF ( ASSOCIATED( grid%ra ) ) THEN 
  DEALLOCATE(grid%ra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6309,&
'frame/module_domain.f: Failed to deallocate grid%ra. ')
 endif
  NULLIFY(grid%ra)
ENDIF
IF ( ASSOCIATED( grid%rs ) ) THEN 
  DEALLOCATE(grid%rs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6317,&
'frame/module_domain.f: Failed to deallocate grid%rs. ')
 endif
  NULLIFY(grid%rs)
ENDIF
IF ( ASSOCIATED( grid%lai ) ) THEN 
  DEALLOCATE(grid%lai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6325,&
'frame/module_domain.f: Failed to deallocate grid%lai. ')
 endif
  NULLIFY(grid%lai)
ENDIF
IF ( ASSOCIATED( grid%vegf_px ) ) THEN 
  DEALLOCATE(grid%vegf_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6333,&
'frame/module_domain.f: Failed to deallocate grid%vegf_px. ')
 endif
  NULLIFY(grid%vegf_px)
ENDIF
IF ( ASSOCIATED( grid%t2obs ) ) THEN 
  DEALLOCATE(grid%t2obs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6341,&
'frame/module_domain.f: Failed to deallocate grid%t2obs. ')
 endif
  NULLIFY(grid%t2obs)
ENDIF
IF ( ASSOCIATED( grid%q2obs ) ) THEN 
  DEALLOCATE(grid%q2obs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6349,&
'frame/module_domain.f: Failed to deallocate grid%q2obs. ')
 endif
  NULLIFY(grid%q2obs)
ENDIF
IF ( ASSOCIATED( grid%imperv ) ) THEN 
  DEALLOCATE(grid%imperv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6357,&
'frame/module_domain.f: Failed to deallocate grid%imperv. ')
 endif
  NULLIFY(grid%imperv)
ENDIF
IF ( ASSOCIATED( grid%canfra ) ) THEN 
  DEALLOCATE(grid%canfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6365,&
'frame/module_domain.f: Failed to deallocate grid%canfra. ')
 endif
  NULLIFY(grid%canfra)
ENDIF
IF ( ASSOCIATED( grid%lai_px ) ) THEN 
  DEALLOCATE(grid%lai_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6373,&
'frame/module_domain.f: Failed to deallocate grid%lai_px. ')
 endif
  NULLIFY(grid%lai_px)
ENDIF
IF ( ASSOCIATED( grid%wwlt_px ) ) THEN 
  DEALLOCATE(grid%wwlt_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6381,&
'frame/module_domain.f: Failed to deallocate grid%wwlt_px. ')
 endif
  NULLIFY(grid%wwlt_px)
ENDIF
IF ( ASSOCIATED( grid%wfc_px ) ) THEN 
  DEALLOCATE(grid%wfc_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6389,&
'frame/module_domain.f: Failed to deallocate grid%wfc_px. ')
 endif
  NULLIFY(grid%wfc_px)
ENDIF
IF ( ASSOCIATED( grid%wsat_px ) ) THEN 
  DEALLOCATE(grid%wsat_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6397,&
'frame/module_domain.f: Failed to deallocate grid%wsat_px. ')
 endif
  NULLIFY(grid%wsat_px)
ENDIF
IF ( ASSOCIATED( grid%clay_px ) ) THEN 
  DEALLOCATE(grid%clay_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6405,&
'frame/module_domain.f: Failed to deallocate grid%clay_px. ')
 endif
  NULLIFY(grid%clay_px)
ENDIF
IF ( ASSOCIATED( grid%csand_px ) ) THEN 
  DEALLOCATE(grid%csand_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6413,&
'frame/module_domain.f: Failed to deallocate grid%csand_px. ')
 endif
  NULLIFY(grid%csand_px)
ENDIF
IF ( ASSOCIATED( grid%fmsand_px ) ) THEN 
  DEALLOCATE(grid%fmsand_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6421,&
'frame/module_domain.f: Failed to deallocate grid%fmsand_px. ')
 endif
  NULLIFY(grid%fmsand_px)
ENDIF
IF ( ASSOCIATED( grid%fm ) ) THEN 
  DEALLOCATE(grid%fm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6429,&
'frame/module_domain.f: Failed to deallocate grid%fm. ')
 endif
  NULLIFY(grid%fm)
ENDIF
IF ( ASSOCIATED( grid%fh ) ) THEN 
  DEALLOCATE(grid%fh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6437,&
'frame/module_domain.f: Failed to deallocate grid%fh. ')
 endif
  NULLIFY(grid%fh)
ENDIF
IF ( ASSOCIATED( grid%wspd ) ) THEN 
  DEALLOCATE(grid%wspd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6445,&
'frame/module_domain.f: Failed to deallocate grid%wspd. ')
 endif
  NULLIFY(grid%wspd)
ENDIF
IF ( ASSOCIATED( grid%br ) ) THEN 
  DEALLOCATE(grid%br,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6453,&
'frame/module_domain.f: Failed to deallocate grid%br. ')
 endif
  NULLIFY(grid%br)
ENDIF
IF ( ASSOCIATED( grid%zol ) ) THEN 
  DEALLOCATE(grid%zol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6461,&
'frame/module_domain.f: Failed to deallocate grid%zol. ')
 endif
  NULLIFY(grid%zol)
ENDIF
IF ( ASSOCIATED( grid%wstar_ysu ) ) THEN 
  DEALLOCATE(grid%wstar_ysu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6469,&
'frame/module_domain.f: Failed to deallocate grid%wstar_ysu. ')
 endif
  NULLIFY(grid%wstar_ysu)
ENDIF
IF ( ASSOCIATED( grid%delta_ysu ) ) THEN 
  DEALLOCATE(grid%delta_ysu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6477,&
'frame/module_domain.f: Failed to deallocate grid%delta_ysu. ')
 endif
  NULLIFY(grid%delta_ysu)
ENDIF
IF ( ASSOCIATED( grid%pek_pbl ) ) THEN 
  DEALLOCATE(grid%pek_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6485,&
'frame/module_domain.f: Failed to deallocate grid%pek_pbl. ')
 endif
  NULLIFY(grid%pek_pbl)
ENDIF
IF ( ASSOCIATED( grid%pep_pbl ) ) THEN 
  DEALLOCATE(grid%pep_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6493,&
'frame/module_domain.f: Failed to deallocate grid%pep_pbl. ')
 endif
  NULLIFY(grid%pep_pbl)
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6501,&
'frame/module_domain.f: Failed to deallocate grid%exch_h. ')
 endif
  NULLIFY(grid%exch_h)
ENDIF
IF ( ASSOCIATED( grid%exch_m ) ) THEN 
  DEALLOCATE(grid%exch_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6509,&
'frame/module_domain.f: Failed to deallocate grid%exch_m. ')
 endif
  NULLIFY(grid%exch_m)
ENDIF
IF ( ASSOCIATED( grid%ct ) ) THEN 
  DEALLOCATE(grid%ct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6517,&
'frame/module_domain.f: Failed to deallocate grid%ct. ')
 endif
  NULLIFY(grid%ct)
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6525,&
'frame/module_domain.f: Failed to deallocate grid%thz0. ')
 endif
  NULLIFY(grid%thz0)
ENDIF
IF ( ASSOCIATED( grid%z0 ) ) THEN 
  DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6533,&
'frame/module_domain.f: Failed to deallocate grid%z0. ')
 endif
  NULLIFY(grid%z0)
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6541,&
'frame/module_domain.f: Failed to deallocate grid%qz0. ')
 endif
  NULLIFY(grid%qz0)
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6549,&
'frame/module_domain.f: Failed to deallocate grid%uz0. ')
 endif
  NULLIFY(grid%uz0)
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6557,&
'frame/module_domain.f: Failed to deallocate grid%vz0. ')
 endif
  NULLIFY(grid%vz0)
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6565,&
'frame/module_domain.f: Failed to deallocate grid%qsfc. ')
 endif
  NULLIFY(grid%qsfc)
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6573,&
'frame/module_domain.f: Failed to deallocate grid%akhs. ')
 endif
  NULLIFY(grid%akhs)
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6581,&
'frame/module_domain.f: Failed to deallocate grid%akms. ')
 endif
  NULLIFY(grid%akms)
ENDIF
IF ( ASSOCIATED( grid%kpbl ) ) THEN 
  DEALLOCATE(grid%kpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6589,&
'frame/module_domain.f: Failed to deallocate grid%kpbl. ')
 endif
  NULLIFY(grid%kpbl)
ENDIF
IF ( ASSOCIATED( grid%u10e ) ) THEN 
  DEALLOCATE(grid%u10e,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6597,&
'frame/module_domain.f: Failed to deallocate grid%u10e. ')
 endif
  NULLIFY(grid%u10e)
ENDIF
IF ( ASSOCIATED( grid%v10e ) ) THEN 
  DEALLOCATE(grid%v10e,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6605,&
'frame/module_domain.f: Failed to deallocate grid%v10e. ')
 endif
  NULLIFY(grid%v10e)
ENDIF
IF ( ASSOCIATED( grid%akpbl ) ) THEN 
  DEALLOCATE(grid%akpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6613,&
'frame/module_domain.f: Failed to deallocate grid%akpbl. ')
 endif
  NULLIFY(grid%akpbl)
ENDIF
IF ( ASSOCIATED( grid%tshltr ) ) THEN 
  DEALLOCATE(grid%tshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6621,&
'frame/module_domain.f: Failed to deallocate grid%tshltr. ')
 endif
  NULLIFY(grid%tshltr)
ENDIF
IF ( ASSOCIATED( grid%qshltr ) ) THEN 
  DEALLOCATE(grid%qshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6629,&
'frame/module_domain.f: Failed to deallocate grid%qshltr. ')
 endif
  NULLIFY(grid%qshltr)
ENDIF
IF ( ASSOCIATED( grid%pshltr ) ) THEN 
  DEALLOCATE(grid%pshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6637,&
'frame/module_domain.f: Failed to deallocate grid%pshltr. ')
 endif
  NULLIFY(grid%pshltr)
ENDIF
IF ( ASSOCIATED( grid%th10 ) ) THEN 
  DEALLOCATE(grid%th10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6645,&
'frame/module_domain.f: Failed to deallocate grid%th10. ')
 endif
  NULLIFY(grid%th10)
ENDIF
IF ( ASSOCIATED( grid%q10 ) ) THEN 
  DEALLOCATE(grid%q10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6653,&
'frame/module_domain.f: Failed to deallocate grid%q10. ')
 endif
  NULLIFY(grid%q10)
ENDIF
IF ( ASSOCIATED( grid%massflux_edkf ) ) THEN 
  DEALLOCATE(grid%massflux_edkf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6661,&
'frame/module_domain.f: Failed to deallocate grid%massflux_edkf. ')
 endif
  NULLIFY(grid%massflux_edkf)
ENDIF
IF ( ASSOCIATED( grid%entr_edkf ) ) THEN 
  DEALLOCATE(grid%entr_edkf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6669,&
'frame/module_domain.f: Failed to deallocate grid%entr_edkf. ')
 endif
  NULLIFY(grid%entr_edkf)
ENDIF
IF ( ASSOCIATED( grid%detr_edkf ) ) THEN 
  DEALLOCATE(grid%detr_edkf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6677,&
'frame/module_domain.f: Failed to deallocate grid%detr_edkf. ')
 endif
  NULLIFY(grid%detr_edkf)
ENDIF
IF ( ASSOCIATED( grid%thl_up ) ) THEN 
  DEALLOCATE(grid%thl_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6685,&
'frame/module_domain.f: Failed to deallocate grid%thl_up. ')
 endif
  NULLIFY(grid%thl_up)
ENDIF
IF ( ASSOCIATED( grid%thv_up ) ) THEN 
  DEALLOCATE(grid%thv_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6693,&
'frame/module_domain.f: Failed to deallocate grid%thv_up. ')
 endif
  NULLIFY(grid%thv_up)
ENDIF
IF ( ASSOCIATED( grid%rv_up ) ) THEN 
  DEALLOCATE(grid%rv_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6701,&
'frame/module_domain.f: Failed to deallocate grid%rv_up. ')
 endif
  NULLIFY(grid%rv_up)
ENDIF
IF ( ASSOCIATED( grid%rt_up ) ) THEN 
  DEALLOCATE(grid%rt_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6709,&
'frame/module_domain.f: Failed to deallocate grid%rt_up. ')
 endif
  NULLIFY(grid%rt_up)
ENDIF
IF ( ASSOCIATED( grid%rc_up ) ) THEN 
  DEALLOCATE(grid%rc_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6717,&
'frame/module_domain.f: Failed to deallocate grid%rc_up. ')
 endif
  NULLIFY(grid%rc_up)
ENDIF
IF ( ASSOCIATED( grid%u_up ) ) THEN 
  DEALLOCATE(grid%u_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6725,&
'frame/module_domain.f: Failed to deallocate grid%u_up. ')
 endif
  NULLIFY(grid%u_up)
ENDIF
IF ( ASSOCIATED( grid%v_up ) ) THEN 
  DEALLOCATE(grid%v_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6733,&
'frame/module_domain.f: Failed to deallocate grid%v_up. ')
 endif
  NULLIFY(grid%v_up)
ENDIF
IF ( ASSOCIATED( grid%frac_up ) ) THEN 
  DEALLOCATE(grid%frac_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6741,&
'frame/module_domain.f: Failed to deallocate grid%frac_up. ')
 endif
  NULLIFY(grid%frac_up)
ENDIF
IF ( ASSOCIATED( grid%rc_mf ) ) THEN 
  DEALLOCATE(grid%rc_mf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6749,&
'frame/module_domain.f: Failed to deallocate grid%rc_mf. ')
 endif
  NULLIFY(grid%rc_mf)
ENDIF
IF ( ASSOCIATED( grid%te_temf ) ) THEN 
  DEALLOCATE(grid%te_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6757,&
'frame/module_domain.f: Failed to deallocate grid%te_temf. ')
 endif
  NULLIFY(grid%te_temf)
ENDIF
IF ( ASSOCIATED( grid%kh_temf ) ) THEN 
  DEALLOCATE(grid%kh_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6765,&
'frame/module_domain.f: Failed to deallocate grid%kh_temf. ')
 endif
  NULLIFY(grid%kh_temf)
ENDIF
IF ( ASSOCIATED( grid%km_temf ) ) THEN 
  DEALLOCATE(grid%km_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6773,&
'frame/module_domain.f: Failed to deallocate grid%km_temf. ')
 endif
  NULLIFY(grid%km_temf)
ENDIF
IF ( ASSOCIATED( grid%shf_temf ) ) THEN 
  DEALLOCATE(grid%shf_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6781,&
'frame/module_domain.f: Failed to deallocate grid%shf_temf. ')
 endif
  NULLIFY(grid%shf_temf)
ENDIF
IF ( ASSOCIATED( grid%qf_temf ) ) THEN 
  DEALLOCATE(grid%qf_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6789,&
'frame/module_domain.f: Failed to deallocate grid%qf_temf. ')
 endif
  NULLIFY(grid%qf_temf)
ENDIF
IF ( ASSOCIATED( grid%uw_temf ) ) THEN 
  DEALLOCATE(grid%uw_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6797,&
'frame/module_domain.f: Failed to deallocate grid%uw_temf. ')
 endif
  NULLIFY(grid%uw_temf)
ENDIF
IF ( ASSOCIATED( grid%vw_temf ) ) THEN 
  DEALLOCATE(grid%vw_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6805,&
'frame/module_domain.f: Failed to deallocate grid%vw_temf. ')
 endif
  NULLIFY(grid%vw_temf)
ENDIF
IF ( ASSOCIATED( grid%wupd_temf ) ) THEN 
  DEALLOCATE(grid%wupd_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6813,&
'frame/module_domain.f: Failed to deallocate grid%wupd_temf. ')
 endif
  NULLIFY(grid%wupd_temf)
ENDIF
IF ( ASSOCIATED( grid%mf_temf ) ) THEN 
  DEALLOCATE(grid%mf_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6821,&
'frame/module_domain.f: Failed to deallocate grid%mf_temf. ')
 endif
  NULLIFY(grid%mf_temf)
ENDIF
IF ( ASSOCIATED( grid%thup_temf ) ) THEN 
  DEALLOCATE(grid%thup_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6829,&
'frame/module_domain.f: Failed to deallocate grid%thup_temf. ')
 endif
  NULLIFY(grid%thup_temf)
ENDIF
IF ( ASSOCIATED( grid%qtup_temf ) ) THEN 
  DEALLOCATE(grid%qtup_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6837,&
'frame/module_domain.f: Failed to deallocate grid%qtup_temf. ')
 endif
  NULLIFY(grid%qtup_temf)
ENDIF
IF ( ASSOCIATED( grid%qlup_temf ) ) THEN 
  DEALLOCATE(grid%qlup_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6845,&
'frame/module_domain.f: Failed to deallocate grid%qlup_temf. ')
 endif
  NULLIFY(grid%qlup_temf)
ENDIF
IF ( ASSOCIATED( grid%cf3d_temf ) ) THEN 
  DEALLOCATE(grid%cf3d_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6853,&
'frame/module_domain.f: Failed to deallocate grid%cf3d_temf. ')
 endif
  NULLIFY(grid%cf3d_temf)
ENDIF
IF ( ASSOCIATED( grid%hd_temf ) ) THEN 
  DEALLOCATE(grid%hd_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6861,&
'frame/module_domain.f: Failed to deallocate grid%hd_temf. ')
 endif
  NULLIFY(grid%hd_temf)
ENDIF
IF ( ASSOCIATED( grid%lcl_temf ) ) THEN 
  DEALLOCATE(grid%lcl_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6869,&
'frame/module_domain.f: Failed to deallocate grid%lcl_temf. ')
 endif
  NULLIFY(grid%lcl_temf)
ENDIF
IF ( ASSOCIATED( grid%hct_temf ) ) THEN 
  DEALLOCATE(grid%hct_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6877,&
'frame/module_domain.f: Failed to deallocate grid%hct_temf. ')
 endif
  NULLIFY(grid%hct_temf)
ENDIF
IF ( ASSOCIATED( grid%cfm_temf ) ) THEN 
  DEALLOCATE(grid%cfm_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6885,&
'frame/module_domain.f: Failed to deallocate grid%cfm_temf. ')
 endif
  NULLIFY(grid%cfm_temf)
ENDIF
IF ( ASSOCIATED( grid%wm_temf ) ) THEN 
  DEALLOCATE(grid%wm_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6893,&
'frame/module_domain.f: Failed to deallocate grid%wm_temf. ')
 endif
  NULLIFY(grid%wm_temf)
ENDIF
IF ( ASSOCIATED( grid%qke ) ) THEN 
  DEALLOCATE(grid%qke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6901,&
'frame/module_domain.f: Failed to deallocate grid%qke. ')
 endif
  NULLIFY(grid%qke)
ENDIF
IF ( ASSOCIATED( grid%qshear ) ) THEN 
  DEALLOCATE(grid%qshear,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6909,&
'frame/module_domain.f: Failed to deallocate grid%qshear. ')
 endif
  NULLIFY(grid%qshear)
ENDIF
IF ( ASSOCIATED( grid%qbuoy ) ) THEN 
  DEALLOCATE(grid%qbuoy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6917,&
'frame/module_domain.f: Failed to deallocate grid%qbuoy. ')
 endif
  NULLIFY(grid%qbuoy)
ENDIF
IF ( ASSOCIATED( grid%qdiss ) ) THEN 
  DEALLOCATE(grid%qdiss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6925,&
'frame/module_domain.f: Failed to deallocate grid%qdiss. ')
 endif
  NULLIFY(grid%qdiss)
ENDIF
IF ( ASSOCIATED( grid%qwt ) ) THEN 
  DEALLOCATE(grid%qwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6933,&
'frame/module_domain.f: Failed to deallocate grid%qwt. ')
 endif
  NULLIFY(grid%qwt)
ENDIF
IF ( ASSOCIATED( grid%dqke ) ) THEN 
  DEALLOCATE(grid%dqke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6941,&
'frame/module_domain.f: Failed to deallocate grid%dqke. ')
 endif
  NULLIFY(grid%dqke)
ENDIF
IF ( ASSOCIATED( grid%tsq ) ) THEN 
  DEALLOCATE(grid%tsq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6949,&
'frame/module_domain.f: Failed to deallocate grid%tsq. ')
 endif
  NULLIFY(grid%tsq)
ENDIF
IF ( ASSOCIATED( grid%qsq ) ) THEN 
  DEALLOCATE(grid%qsq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6957,&
'frame/module_domain.f: Failed to deallocate grid%qsq. ')
 endif
  NULLIFY(grid%qsq)
ENDIF
IF ( ASSOCIATED( grid%cov ) ) THEN 
  DEALLOCATE(grid%cov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6965,&
'frame/module_domain.f: Failed to deallocate grid%cov. ')
 endif
  NULLIFY(grid%cov)
ENDIF
IF ( ASSOCIATED( grid%sh3d ) ) THEN 
  DEALLOCATE(grid%sh3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6973,&
'frame/module_domain.f: Failed to deallocate grid%sh3d. ')
 endif
  NULLIFY(grid%sh3d)
ENDIF
IF ( ASSOCIATED( grid%sm3d ) ) THEN 
  DEALLOCATE(grid%sm3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6981,&
'frame/module_domain.f: Failed to deallocate grid%sm3d. ')
 endif
  NULLIFY(grid%sm3d)
ENDIF
IF ( ASSOCIATED( grid%ch ) ) THEN 
  DEALLOCATE(grid%ch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6989,&
'frame/module_domain.f: Failed to deallocate grid%ch. ')
 endif
  NULLIFY(grid%ch)
ENDIF
IF ( ASSOCIATED( grid%edmf_a ) ) THEN 
  DEALLOCATE(grid%edmf_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6997,&
'frame/module_domain.f: Failed to deallocate grid%edmf_a. ')
 endif
  NULLIFY(grid%edmf_a)
ENDIF
IF ( ASSOCIATED( grid%edmf_w ) ) THEN 
  DEALLOCATE(grid%edmf_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7005,&
'frame/module_domain.f: Failed to deallocate grid%edmf_w. ')
 endif
  NULLIFY(grid%edmf_w)
ENDIF
IF ( ASSOCIATED( grid%edmf_thl ) ) THEN 
  DEALLOCATE(grid%edmf_thl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7013,&
'frame/module_domain.f: Failed to deallocate grid%edmf_thl. ')
 endif
  NULLIFY(grid%edmf_thl)
ENDIF
IF ( ASSOCIATED( grid%edmf_qt ) ) THEN 
  DEALLOCATE(grid%edmf_qt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7021,&
'frame/module_domain.f: Failed to deallocate grid%edmf_qt. ')
 endif
  NULLIFY(grid%edmf_qt)
ENDIF
IF ( ASSOCIATED( grid%edmf_ent ) ) THEN 
  DEALLOCATE(grid%edmf_ent,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7029,&
'frame/module_domain.f: Failed to deallocate grid%edmf_ent. ')
 endif
  NULLIFY(grid%edmf_ent)
ENDIF
IF ( ASSOCIATED( grid%edmf_qc ) ) THEN 
  DEALLOCATE(grid%edmf_qc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7037,&
'frame/module_domain.f: Failed to deallocate grid%edmf_qc. ')
 endif
  NULLIFY(grid%edmf_qc)
ENDIF
IF ( ASSOCIATED( grid%sub_thl3d ) ) THEN 
  DEALLOCATE(grid%sub_thl3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7045,&
'frame/module_domain.f: Failed to deallocate grid%sub_thl3d. ')
 endif
  NULLIFY(grid%sub_thl3d)
ENDIF
IF ( ASSOCIATED( grid%sub_sqv3d ) ) THEN 
  DEALLOCATE(grid%sub_sqv3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7053,&
'frame/module_domain.f: Failed to deallocate grid%sub_sqv3d. ')
 endif
  NULLIFY(grid%sub_sqv3d)
ENDIF
IF ( ASSOCIATED( grid%det_thl3d ) ) THEN 
  DEALLOCATE(grid%det_thl3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7061,&
'frame/module_domain.f: Failed to deallocate grid%det_thl3d. ')
 endif
  NULLIFY(grid%det_thl3d)
ENDIF
IF ( ASSOCIATED( grid%det_sqv3d ) ) THEN 
  DEALLOCATE(grid%det_sqv3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7069,&
'frame/module_domain.f: Failed to deallocate grid%det_sqv3d. ')
 endif
  NULLIFY(grid%det_sqv3d)
ENDIF
IF ( ASSOCIATED( grid%ktop_plume ) ) THEN 
  DEALLOCATE(grid%ktop_plume,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7077,&
'frame/module_domain.f: Failed to deallocate grid%ktop_plume. ')
 endif
  NULLIFY(grid%ktop_plume)
ENDIF
IF ( ASSOCIATED( grid%maxmf ) ) THEN 
  DEALLOCATE(grid%maxmf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7085,&
'frame/module_domain.f: Failed to deallocate grid%maxmf. ')
 endif
  NULLIFY(grid%maxmf)
ENDIF
IF ( ASSOCIATED( grid%maxwidth ) ) THEN 
  DEALLOCATE(grid%maxwidth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7093,&
'frame/module_domain.f: Failed to deallocate grid%maxwidth. ')
 endif
  NULLIFY(grid%maxwidth)
ENDIF
IF ( ASSOCIATED( grid%ztop_plume ) ) THEN 
  DEALLOCATE(grid%ztop_plume,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7101,&
'frame/module_domain.f: Failed to deallocate grid%ztop_plume. ')
 endif
  NULLIFY(grid%ztop_plume)
ENDIF
IF ( ASSOCIATED( grid%fgdp ) ) THEN 
  DEALLOCATE(grid%fgdp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7109,&
'frame/module_domain.f: Failed to deallocate grid%fgdp. ')
 endif
  NULLIFY(grid%fgdp)
ENDIF
IF ( ASSOCIATED( grid%dfgdp ) ) THEN 
  DEALLOCATE(grid%dfgdp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7117,&
'frame/module_domain.f: Failed to deallocate grid%dfgdp. ')
 endif
  NULLIFY(grid%dfgdp)
ENDIF
IF ( ASSOCIATED( grid%vdfg ) ) THEN 
  DEALLOCATE(grid%vdfg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7125,&
'frame/module_domain.f: Failed to deallocate grid%vdfg. ')
 endif
  NULLIFY(grid%vdfg)
ENDIF
IF ( ASSOCIATED( grid%exch_tke ) ) THEN 
  DEALLOCATE(grid%exch_tke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7133,&
'frame/module_domain.f: Failed to deallocate grid%exch_tke. ')
 endif
  NULLIFY(grid%exch_tke)
ENDIF
IF ( ASSOCIATED( grid%dtaux3d ) ) THEN 
  DEALLOCATE(grid%dtaux3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7141,&
'frame/module_domain.f: Failed to deallocate grid%dtaux3d. ')
 endif
  NULLIFY(grid%dtaux3d)
ENDIF
IF ( ASSOCIATED( grid%dtauy3d ) ) THEN 
  DEALLOCATE(grid%dtauy3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7149,&
'frame/module_domain.f: Failed to deallocate grid%dtauy3d. ')
 endif
  NULLIFY(grid%dtauy3d)
ENDIF
IF ( ASSOCIATED( grid%dusfcg ) ) THEN 
  DEALLOCATE(grid%dusfcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7157,&
'frame/module_domain.f: Failed to deallocate grid%dusfcg. ')
 endif
  NULLIFY(grid%dusfcg)
ENDIF
IF ( ASSOCIATED( grid%dvsfcg ) ) THEN 
  DEALLOCATE(grid%dvsfcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7165,&
'frame/module_domain.f: Failed to deallocate grid%dvsfcg. ')
 endif
  NULLIFY(grid%dvsfcg)
ENDIF
IF ( ASSOCIATED( grid%var2d ) ) THEN 
  DEALLOCATE(grid%var2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7173,&
'frame/module_domain.f: Failed to deallocate grid%var2d. ')
 endif
  NULLIFY(grid%var2d)
ENDIF
IF ( ASSOCIATED( grid%oc12d ) ) THEN 
  DEALLOCATE(grid%oc12d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7181,&
'frame/module_domain.f: Failed to deallocate grid%oc12d. ')
 endif
  NULLIFY(grid%oc12d)
ENDIF
IF ( ASSOCIATED( grid%oa1 ) ) THEN 
  DEALLOCATE(grid%oa1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7189,&
'frame/module_domain.f: Failed to deallocate grid%oa1. ')
 endif
  NULLIFY(grid%oa1)
ENDIF
IF ( ASSOCIATED( grid%oa2 ) ) THEN 
  DEALLOCATE(grid%oa2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7197,&
'frame/module_domain.f: Failed to deallocate grid%oa2. ')
 endif
  NULLIFY(grid%oa2)
ENDIF
IF ( ASSOCIATED( grid%oa3 ) ) THEN 
  DEALLOCATE(grid%oa3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7205,&
'frame/module_domain.f: Failed to deallocate grid%oa3. ')
 endif
  NULLIFY(grid%oa3)
ENDIF
IF ( ASSOCIATED( grid%oa4 ) ) THEN 
  DEALLOCATE(grid%oa4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7213,&
'frame/module_domain.f: Failed to deallocate grid%oa4. ')
 endif
  NULLIFY(grid%oa4)
ENDIF
IF ( ASSOCIATED( grid%ol1 ) ) THEN 
  DEALLOCATE(grid%ol1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7221,&
'frame/module_domain.f: Failed to deallocate grid%ol1. ')
 endif
  NULLIFY(grid%ol1)
ENDIF
IF ( ASSOCIATED( grid%ol2 ) ) THEN 
  DEALLOCATE(grid%ol2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7229,&
'frame/module_domain.f: Failed to deallocate grid%ol2. ')
 endif
  NULLIFY(grid%ol2)
ENDIF
IF ( ASSOCIATED( grid%ol3 ) ) THEN 
  DEALLOCATE(grid%ol3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7237,&
'frame/module_domain.f: Failed to deallocate grid%ol3. ')
 endif
  NULLIFY(grid%ol3)
ENDIF
IF ( ASSOCIATED( grid%ol4 ) ) THEN 
  DEALLOCATE(grid%ol4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7245,&
'frame/module_domain.f: Failed to deallocate grid%ol4. ')
 endif
  NULLIFY(grid%ol4)
ENDIF
IF ( ASSOCIATED( grid%dtaux3d_ls ) ) THEN 
  DEALLOCATE(grid%dtaux3d_ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7253,&
'frame/module_domain.f: Failed to deallocate grid%dtaux3d_ls. ')
 endif
  NULLIFY(grid%dtaux3d_ls)
ENDIF
IF ( ASSOCIATED( grid%dtauy3d_ls ) ) THEN 
  DEALLOCATE(grid%dtauy3d_ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7261,&
'frame/module_domain.f: Failed to deallocate grid%dtauy3d_ls. ')
 endif
  NULLIFY(grid%dtauy3d_ls)
ENDIF
IF ( ASSOCIATED( grid%dtaux3d_bl ) ) THEN 
  DEALLOCATE(grid%dtaux3d_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7269,&
'frame/module_domain.f: Failed to deallocate grid%dtaux3d_bl. ')
 endif
  NULLIFY(grid%dtaux3d_bl)
ENDIF
IF ( ASSOCIATED( grid%dtauy3d_bl ) ) THEN 
  DEALLOCATE(grid%dtauy3d_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7277,&
'frame/module_domain.f: Failed to deallocate grid%dtauy3d_bl. ')
 endif
  NULLIFY(grid%dtauy3d_bl)
ENDIF
IF ( ASSOCIATED( grid%dtaux3d_ss ) ) THEN 
  DEALLOCATE(grid%dtaux3d_ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7285,&
'frame/module_domain.f: Failed to deallocate grid%dtaux3d_ss. ')
 endif
  NULLIFY(grid%dtaux3d_ss)
ENDIF
IF ( ASSOCIATED( grid%dtauy3d_ss ) ) THEN 
  DEALLOCATE(grid%dtauy3d_ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7293,&
'frame/module_domain.f: Failed to deallocate grid%dtauy3d_ss. ')
 endif
  NULLIFY(grid%dtauy3d_ss)
ENDIF
IF ( ASSOCIATED( grid%dtaux3d_fd ) ) THEN 
  DEALLOCATE(grid%dtaux3d_fd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7301,&
'frame/module_domain.f: Failed to deallocate grid%dtaux3d_fd. ')
 endif
  NULLIFY(grid%dtaux3d_fd)
ENDIF
IF ( ASSOCIATED( grid%dtauy3d_fd ) ) THEN 
  DEALLOCATE(grid%dtauy3d_fd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7309,&
'frame/module_domain.f: Failed to deallocate grid%dtauy3d_fd. ')
 endif
  NULLIFY(grid%dtauy3d_fd)
ENDIF
IF ( ASSOCIATED( grid%dusfcg_ls ) ) THEN 
  DEALLOCATE(grid%dusfcg_ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7317,&
'frame/module_domain.f: Failed to deallocate grid%dusfcg_ls. ')
 endif
  NULLIFY(grid%dusfcg_ls)
ENDIF
IF ( ASSOCIATED( grid%dvsfcg_ls ) ) THEN 
  DEALLOCATE(grid%dvsfcg_ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7325,&
'frame/module_domain.f: Failed to deallocate grid%dvsfcg_ls. ')
 endif
  NULLIFY(grid%dvsfcg_ls)
ENDIF
IF ( ASSOCIATED( grid%dusfcg_bl ) ) THEN 
  DEALLOCATE(grid%dusfcg_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7333,&
'frame/module_domain.f: Failed to deallocate grid%dusfcg_bl. ')
 endif
  NULLIFY(grid%dusfcg_bl)
ENDIF
IF ( ASSOCIATED( grid%dvsfcg_bl ) ) THEN 
  DEALLOCATE(grid%dvsfcg_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7341,&
'frame/module_domain.f: Failed to deallocate grid%dvsfcg_bl. ')
 endif
  NULLIFY(grid%dvsfcg_bl)
ENDIF
IF ( ASSOCIATED( grid%dusfcg_ss ) ) THEN 
  DEALLOCATE(grid%dusfcg_ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7349,&
'frame/module_domain.f: Failed to deallocate grid%dusfcg_ss. ')
 endif
  NULLIFY(grid%dusfcg_ss)
ENDIF
IF ( ASSOCIATED( grid%dvsfcg_ss ) ) THEN 
  DEALLOCATE(grid%dvsfcg_ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7357,&
'frame/module_domain.f: Failed to deallocate grid%dvsfcg_ss. ')
 endif
  NULLIFY(grid%dvsfcg_ss)
ENDIF
IF ( ASSOCIATED( grid%dusfcg_fd ) ) THEN 
  DEALLOCATE(grid%dusfcg_fd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7365,&
'frame/module_domain.f: Failed to deallocate grid%dusfcg_fd. ')
 endif
  NULLIFY(grid%dusfcg_fd)
ENDIF
IF ( ASSOCIATED( grid%dvsfcg_fd ) ) THEN 
  DEALLOCATE(grid%dvsfcg_fd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7373,&
'frame/module_domain.f: Failed to deallocate grid%dvsfcg_fd. ')
 endif
  NULLIFY(grid%dvsfcg_fd)
ENDIF
IF ( ASSOCIATED( grid%var2dls ) ) THEN 
  DEALLOCATE(grid%var2dls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7381,&
'frame/module_domain.f: Failed to deallocate grid%var2dls. ')
 endif
  NULLIFY(grid%var2dls)
ENDIF
IF ( ASSOCIATED( grid%oc12dls ) ) THEN 
  DEALLOCATE(grid%oc12dls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7389,&
'frame/module_domain.f: Failed to deallocate grid%oc12dls. ')
 endif
  NULLIFY(grid%oc12dls)
ENDIF
IF ( ASSOCIATED( grid%oa1ls ) ) THEN 
  DEALLOCATE(grid%oa1ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7397,&
'frame/module_domain.f: Failed to deallocate grid%oa1ls. ')
 endif
  NULLIFY(grid%oa1ls)
ENDIF
IF ( ASSOCIATED( grid%oa2ls ) ) THEN 
  DEALLOCATE(grid%oa2ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7405,&
'frame/module_domain.f: Failed to deallocate grid%oa2ls. ')
 endif
  NULLIFY(grid%oa2ls)
ENDIF
IF ( ASSOCIATED( grid%oa3ls ) ) THEN 
  DEALLOCATE(grid%oa3ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7413,&
'frame/module_domain.f: Failed to deallocate grid%oa3ls. ')
 endif
  NULLIFY(grid%oa3ls)
ENDIF
IF ( ASSOCIATED( grid%oa4ls ) ) THEN 
  DEALLOCATE(grid%oa4ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7421,&
'frame/module_domain.f: Failed to deallocate grid%oa4ls. ')
 endif
  NULLIFY(grid%oa4ls)
ENDIF
IF ( ASSOCIATED( grid%ol1ls ) ) THEN 
  DEALLOCATE(grid%ol1ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7429,&
'frame/module_domain.f: Failed to deallocate grid%ol1ls. ')
 endif
  NULLIFY(grid%ol1ls)
ENDIF
IF ( ASSOCIATED( grid%ol2ls ) ) THEN 
  DEALLOCATE(grid%ol2ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7437,&
'frame/module_domain.f: Failed to deallocate grid%ol2ls. ')
 endif
  NULLIFY(grid%ol2ls)
ENDIF
IF ( ASSOCIATED( grid%ol3ls ) ) THEN 
  DEALLOCATE(grid%ol3ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7445,&
'frame/module_domain.f: Failed to deallocate grid%ol3ls. ')
 endif
  NULLIFY(grid%ol3ls)
ENDIF
IF ( ASSOCIATED( grid%ol4ls ) ) THEN 
  DEALLOCATE(grid%ol4ls,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7453,&
'frame/module_domain.f: Failed to deallocate grid%ol4ls. ')
 endif
  NULLIFY(grid%ol4ls)
ENDIF
IF ( ASSOCIATED( grid%var2dss ) ) THEN 
  DEALLOCATE(grid%var2dss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7461,&
'frame/module_domain.f: Failed to deallocate grid%var2dss. ')
 endif
  NULLIFY(grid%var2dss)
ENDIF
IF ( ASSOCIATED( grid%oc12dss ) ) THEN 
  DEALLOCATE(grid%oc12dss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7469,&
'frame/module_domain.f: Failed to deallocate grid%oc12dss. ')
 endif
  NULLIFY(grid%oc12dss)
ENDIF
IF ( ASSOCIATED( grid%oa1ss ) ) THEN 
  DEALLOCATE(grid%oa1ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7477,&
'frame/module_domain.f: Failed to deallocate grid%oa1ss. ')
 endif
  NULLIFY(grid%oa1ss)
ENDIF
IF ( ASSOCIATED( grid%oa2ss ) ) THEN 
  DEALLOCATE(grid%oa2ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7485,&
'frame/module_domain.f: Failed to deallocate grid%oa2ss. ')
 endif
  NULLIFY(grid%oa2ss)
ENDIF
IF ( ASSOCIATED( grid%oa3ss ) ) THEN 
  DEALLOCATE(grid%oa3ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7493,&
'frame/module_domain.f: Failed to deallocate grid%oa3ss. ')
 endif
  NULLIFY(grid%oa3ss)
ENDIF
IF ( ASSOCIATED( grid%oa4ss ) ) THEN 
  DEALLOCATE(grid%oa4ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7501,&
'frame/module_domain.f: Failed to deallocate grid%oa4ss. ')
 endif
  NULLIFY(grid%oa4ss)
ENDIF
IF ( ASSOCIATED( grid%ol1ss ) ) THEN 
  DEALLOCATE(grid%ol1ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7509,&
'frame/module_domain.f: Failed to deallocate grid%ol1ss. ')
 endif
  NULLIFY(grid%ol1ss)
ENDIF
IF ( ASSOCIATED( grid%ol2ss ) ) THEN 
  DEALLOCATE(grid%ol2ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7517,&
'frame/module_domain.f: Failed to deallocate grid%ol2ss. ')
 endif
  NULLIFY(grid%ol2ss)
ENDIF
IF ( ASSOCIATED( grid%ol3ss ) ) THEN 
  DEALLOCATE(grid%ol3ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7525,&
'frame/module_domain.f: Failed to deallocate grid%ol3ss. ')
 endif
  NULLIFY(grid%ol3ss)
ENDIF
IF ( ASSOCIATED( grid%ol4ss ) ) THEN 
  DEALLOCATE(grid%ol4ss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7533,&
'frame/module_domain.f: Failed to deallocate grid%ol4ss. ')
 endif
  NULLIFY(grid%ol4ss)
ENDIF
IF ( ASSOCIATED( grid%ctopo ) ) THEN 
  DEALLOCATE(grid%ctopo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7541,&
'frame/module_domain.f: Failed to deallocate grid%ctopo. ')
 endif
  NULLIFY(grid%ctopo)
ENDIF
IF ( ASSOCIATED( grid%ctopo2 ) ) THEN 
  DEALLOCATE(grid%ctopo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7549,&
'frame/module_domain.f: Failed to deallocate grid%ctopo2. ')
 endif
  NULLIFY(grid%ctopo2)
ENDIF
IF ( ASSOCIATED( grid%a_u_bep ) ) THEN 
  DEALLOCATE(grid%a_u_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7557,&
'frame/module_domain.f: Failed to deallocate grid%a_u_bep. ')
 endif
  NULLIFY(grid%a_u_bep)
ENDIF
IF ( ASSOCIATED( grid%a_v_bep ) ) THEN 
  DEALLOCATE(grid%a_v_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7565,&
'frame/module_domain.f: Failed to deallocate grid%a_v_bep. ')
 endif
  NULLIFY(grid%a_v_bep)
ENDIF
IF ( ASSOCIATED( grid%a_t_bep ) ) THEN 
  DEALLOCATE(grid%a_t_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7573,&
'frame/module_domain.f: Failed to deallocate grid%a_t_bep. ')
 endif
  NULLIFY(grid%a_t_bep)
ENDIF
IF ( ASSOCIATED( grid%a_q_bep ) ) THEN 
  DEALLOCATE(grid%a_q_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7581,&
'frame/module_domain.f: Failed to deallocate grid%a_q_bep. ')
 endif
  NULLIFY(grid%a_q_bep)
ENDIF
IF ( ASSOCIATED( grid%a_e_bep ) ) THEN 
  DEALLOCATE(grid%a_e_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7589,&
'frame/module_domain.f: Failed to deallocate grid%a_e_bep. ')
 endif
  NULLIFY(grid%a_e_bep)
ENDIF
IF ( ASSOCIATED( grid%b_u_bep ) ) THEN 
  DEALLOCATE(grid%b_u_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7597,&
'frame/module_domain.f: Failed to deallocate grid%b_u_bep. ')
 endif
  NULLIFY(grid%b_u_bep)
ENDIF
IF ( ASSOCIATED( grid%b_v_bep ) ) THEN 
  DEALLOCATE(grid%b_v_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7605,&
'frame/module_domain.f: Failed to deallocate grid%b_v_bep. ')
 endif
  NULLIFY(grid%b_v_bep)
ENDIF
IF ( ASSOCIATED( grid%b_t_bep ) ) THEN 
  DEALLOCATE(grid%b_t_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7613,&
'frame/module_domain.f: Failed to deallocate grid%b_t_bep. ')
 endif
  NULLIFY(grid%b_t_bep)
ENDIF
IF ( ASSOCIATED( grid%b_q_bep ) ) THEN 
  DEALLOCATE(grid%b_q_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7621,&
'frame/module_domain.f: Failed to deallocate grid%b_q_bep. ')
 endif
  NULLIFY(grid%b_q_bep)
ENDIF
IF ( ASSOCIATED( grid%b_e_bep ) ) THEN 
  DEALLOCATE(grid%b_e_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7629,&
'frame/module_domain.f: Failed to deallocate grid%b_e_bep. ')
 endif
  NULLIFY(grid%b_e_bep)
ENDIF
IF ( ASSOCIATED( grid%dlg_bep ) ) THEN 
  DEALLOCATE(grid%dlg_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7637,&
'frame/module_domain.f: Failed to deallocate grid%dlg_bep. ')
 endif
  NULLIFY(grid%dlg_bep)
ENDIF
IF ( ASSOCIATED( grid%dl_u_bep ) ) THEN 
  DEALLOCATE(grid%dl_u_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7645,&
'frame/module_domain.f: Failed to deallocate grid%dl_u_bep. ')
 endif
  NULLIFY(grid%dl_u_bep)
ENDIF
IF ( ASSOCIATED( grid%sf_bep ) ) THEN 
  DEALLOCATE(grid%sf_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7653,&
'frame/module_domain.f: Failed to deallocate grid%sf_bep. ')
 endif
  NULLIFY(grid%sf_bep)
ENDIF
IF ( ASSOCIATED( grid%vl_bep ) ) THEN 
  DEALLOCATE(grid%vl_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7661,&
'frame/module_domain.f: Failed to deallocate grid%vl_bep. ')
 endif
  NULLIFY(grid%vl_bep)
ENDIF
IF ( ASSOCIATED( grid%tke_pbl ) ) THEN 
  DEALLOCATE(grid%tke_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7669,&
'frame/module_domain.f: Failed to deallocate grid%tke_pbl. ')
 endif
  NULLIFY(grid%tke_pbl)
ENDIF
IF ( ASSOCIATED( grid%el_pbl ) ) THEN 
  DEALLOCATE(grid%el_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7677,&
'frame/module_domain.f: Failed to deallocate grid%el_pbl. ')
 endif
  NULLIFY(grid%el_pbl)
ENDIF
IF ( ASSOCIATED( grid%diss_pbl ) ) THEN 
  DEALLOCATE(grid%diss_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7685,&
'frame/module_domain.f: Failed to deallocate grid%diss_pbl. ')
 endif
  NULLIFY(grid%diss_pbl)
ENDIF
IF ( ASSOCIATED( grid%tpe_pbl ) ) THEN 
  DEALLOCATE(grid%tpe_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7693,&
'frame/module_domain.f: Failed to deallocate grid%tpe_pbl. ')
 endif
  NULLIFY(grid%tpe_pbl)
ENDIF
IF ( ASSOCIATED( grid%pr_pbl ) ) THEN 
  DEALLOCATE(grid%pr_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7701,&
'frame/module_domain.f: Failed to deallocate grid%pr_pbl. ')
 endif
  NULLIFY(grid%pr_pbl)
ENDIF
IF ( ASSOCIATED( grid%wu_tur ) ) THEN 
  DEALLOCATE(grid%wu_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7709,&
'frame/module_domain.f: Failed to deallocate grid%wu_tur. ')
 endif
  NULLIFY(grid%wu_tur)
ENDIF
IF ( ASSOCIATED( grid%wv_tur ) ) THEN 
  DEALLOCATE(grid%wv_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7717,&
'frame/module_domain.f: Failed to deallocate grid%wv_tur. ')
 endif
  NULLIFY(grid%wv_tur)
ENDIF
IF ( ASSOCIATED( grid%wt_tur ) ) THEN 
  DEALLOCATE(grid%wt_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7725,&
'frame/module_domain.f: Failed to deallocate grid%wt_tur. ')
 endif
  NULLIFY(grid%wt_tur)
ENDIF
IF ( ASSOCIATED( grid%wq_tur ) ) THEN 
  DEALLOCATE(grid%wq_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7733,&
'frame/module_domain.f: Failed to deallocate grid%wq_tur. ')
 endif
  NULLIFY(grid%wq_tur)
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7741,&
'frame/module_domain.f: Failed to deallocate grid%htop. ')
 endif
  NULLIFY(grid%htop)
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7749,&
'frame/module_domain.f: Failed to deallocate grid%hbot. ')
 endif
  NULLIFY(grid%hbot)
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7757,&
'frame/module_domain.f: Failed to deallocate grid%htopr. ')
 endif
  NULLIFY(grid%htopr)
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7765,&
'frame/module_domain.f: Failed to deallocate grid%hbotr. ')
 endif
  NULLIFY(grid%hbotr)
ENDIF
IF ( ASSOCIATED( grid%cutop ) ) THEN 
  DEALLOCATE(grid%cutop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7773,&
'frame/module_domain.f: Failed to deallocate grid%cutop. ')
 endif
  NULLIFY(grid%cutop)
ENDIF
IF ( ASSOCIATED( grid%cubot ) ) THEN 
  DEALLOCATE(grid%cubot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7781,&
'frame/module_domain.f: Failed to deallocate grid%cubot. ')
 endif
  NULLIFY(grid%cubot)
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7789,&
'frame/module_domain.f: Failed to deallocate grid%cuppt. ')
 endif
  NULLIFY(grid%cuppt)
ENDIF
IF ( ASSOCIATED( grid%rswtoa ) ) THEN 
  DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7797,&
'frame/module_domain.f: Failed to deallocate grid%rswtoa. ')
 endif
  NULLIFY(grid%rswtoa)
ENDIF
IF ( ASSOCIATED( grid%rlwtoa ) ) THEN 
  DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7805,&
'frame/module_domain.f: Failed to deallocate grid%rlwtoa. ')
 endif
  NULLIFY(grid%rlwtoa)
ENDIF
IF ( ASSOCIATED( grid%czmean ) ) THEN 
  DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7813,&
'frame/module_domain.f: Failed to deallocate grid%czmean. ')
 endif
  NULLIFY(grid%czmean)
ENDIF
IF ( ASSOCIATED( grid%cfracl ) ) THEN 
  DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7821,&
'frame/module_domain.f: Failed to deallocate grid%cfracl. ')
 endif
  NULLIFY(grid%cfracl)
ENDIF
IF ( ASSOCIATED( grid%cfracm ) ) THEN 
  DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7829,&
'frame/module_domain.f: Failed to deallocate grid%cfracm. ')
 endif
  NULLIFY(grid%cfracm)
ENDIF
IF ( ASSOCIATED( grid%cfrach ) ) THEN 
  DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7837,&
'frame/module_domain.f: Failed to deallocate grid%cfrach. ')
 endif
  NULLIFY(grid%cfrach)
ENDIF
IF ( ASSOCIATED( grid%acfrst ) ) THEN 
  DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7845,&
'frame/module_domain.f: Failed to deallocate grid%acfrst. ')
 endif
  NULLIFY(grid%acfrst)
ENDIF
IF ( ASSOCIATED( grid%ncfrst ) ) THEN 
  DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7853,&
'frame/module_domain.f: Failed to deallocate grid%ncfrst. ')
 endif
  NULLIFY(grid%ncfrst)
ENDIF
IF ( ASSOCIATED( grid%acfrcv ) ) THEN 
  DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7861,&
'frame/module_domain.f: Failed to deallocate grid%acfrcv. ')
 endif
  NULLIFY(grid%acfrcv)
ENDIF
IF ( ASSOCIATED( grid%ncfrcv ) ) THEN 
  DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7869,&
'frame/module_domain.f: Failed to deallocate grid%ncfrcv. ')
 endif
  NULLIFY(grid%ncfrcv)
ENDIF
IF ( ASSOCIATED( grid%o3rad ) ) THEN 
  DEALLOCATE(grid%o3rad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7877,&
'frame/module_domain.f: Failed to deallocate grid%o3rad. ')
 endif
  NULLIFY(grid%o3rad)
ENDIF
IF ( ASSOCIATED( grid%o3_gfs_du ) ) THEN 
  DEALLOCATE(grid%o3_gfs_du,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7885,&
'frame/module_domain.f: Failed to deallocate grid%o3_gfs_du. ')
 endif
  NULLIFY(grid%o3_gfs_du)
ENDIF
IF ( ASSOCIATED( grid%aerodm ) ) THEN 
  DEALLOCATE(grid%aerodm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7893,&
'frame/module_domain.f: Failed to deallocate grid%aerodm. ')
 endif
  NULLIFY(grid%aerodm)
ENDIF
IF ( ASSOCIATED( grid%pina ) ) THEN 
  DEALLOCATE(grid%pina,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7901,&
'frame/module_domain.f: Failed to deallocate grid%pina. ')
 endif
  NULLIFY(grid%pina)
ENDIF
IF ( ASSOCIATED( grid%aerod ) ) THEN 
  DEALLOCATE(grid%aerod,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7909,&
'frame/module_domain.f: Failed to deallocate grid%aerod. ')
 endif
  NULLIFY(grid%aerod)
ENDIF
IF ( ASSOCIATED( grid%aodtot ) ) THEN 
  DEALLOCATE(grid%aodtot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7917,&
'frame/module_domain.f: Failed to deallocate grid%aodtot. ')
 endif
  NULLIFY(grid%aodtot)
ENDIF
IF ( ASSOCIATED( grid%aeromcu ) ) THEN 
  DEALLOCATE(grid%aeromcu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7925,&
'frame/module_domain.f: Failed to deallocate grid%aeromcu. ')
 endif
  NULLIFY(grid%aeromcu)
ENDIF
IF ( ASSOCIATED( grid%aeropcu ) ) THEN 
  DEALLOCATE(grid%aeropcu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7933,&
'frame/module_domain.f: Failed to deallocate grid%aeropcu. ')
 endif
  NULLIFY(grid%aeropcu)
ENDIF
IF ( ASSOCIATED( grid%aerocu ) ) THEN 
  DEALLOCATE(grid%aerocu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7941,&
'frame/module_domain.f: Failed to deallocate grid%aerocu. ')
 endif
  NULLIFY(grid%aerocu)
ENDIF
IF ( ASSOCIATED( grid%aerovar ) ) THEN 
  DEALLOCATE(grid%aerovar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7949,&
'frame/module_domain.f: Failed to deallocate grid%aerovar. ')
 endif
  NULLIFY(grid%aerovar)
ENDIF
IF ( ASSOCIATED( grid%ozmixm ) ) THEN 
  DEALLOCATE(grid%ozmixm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7957,&
'frame/module_domain.f: Failed to deallocate grid%ozmixm. ')
 endif
  NULLIFY(grid%ozmixm)
ENDIF
IF ( ASSOCIATED( grid%pin ) ) THEN 
  DEALLOCATE(grid%pin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7965,&
'frame/module_domain.f: Failed to deallocate grid%pin. ')
 endif
  NULLIFY(grid%pin)
ENDIF
IF ( ASSOCIATED( grid%m_ps_1 ) ) THEN 
  DEALLOCATE(grid%m_ps_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7973,&
'frame/module_domain.f: Failed to deallocate grid%m_ps_1. ')
 endif
  NULLIFY(grid%m_ps_1)
ENDIF
IF ( ASSOCIATED( grid%m_ps_2 ) ) THEN 
  DEALLOCATE(grid%m_ps_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7981,&
'frame/module_domain.f: Failed to deallocate grid%m_ps_2. ')
 endif
  NULLIFY(grid%m_ps_2)
ENDIF
IF ( ASSOCIATED( grid%aerosolc_1 ) ) THEN 
  DEALLOCATE(grid%aerosolc_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7989,&
'frame/module_domain.f: Failed to deallocate grid%aerosolc_1. ')
 endif
  NULLIFY(grid%aerosolc_1)
ENDIF
IF ( ASSOCIATED( grid%aerosolc_2 ) ) THEN 
  DEALLOCATE(grid%aerosolc_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7997,&
'frame/module_domain.f: Failed to deallocate grid%aerosolc_2. ')
 endif
  NULLIFY(grid%aerosolc_2)
ENDIF
IF ( ASSOCIATED( grid%m_hybi ) ) THEN 
  DEALLOCATE(grid%m_hybi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8005,&
'frame/module_domain.f: Failed to deallocate grid%m_hybi. ')
 endif
  NULLIFY(grid%m_hybi)
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8013,&
'frame/module_domain.f: Failed to deallocate grid%f_ice_phy. ')
 endif
  NULLIFY(grid%f_ice_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8021,&
'frame/module_domain.f: Failed to deallocate grid%f_rain_phy. ')
 endif
  NULLIFY(grid%f_rain_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8029,&
'frame/module_domain.f: Failed to deallocate grid%f_rimef_phy. ')
 endif
  NULLIFY(grid%f_rimef_phy)
ENDIF
IF ( ASSOCIATED( grid%qndropsource ) ) THEN 
  DEALLOCATE(grid%qndropsource,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8037,&
'frame/module_domain.f: Failed to deallocate grid%qndropsource. ')
 endif
  NULLIFY(grid%qndropsource)
ENDIF
IF ( ASSOCIATED( grid%om_tmp ) ) THEN 
  DEALLOCATE(grid%om_tmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8045,&
'frame/module_domain.f: Failed to deallocate grid%om_tmp. ')
 endif
  NULLIFY(grid%om_tmp)
ENDIF
IF ( ASSOCIATED( grid%om_s ) ) THEN 
  DEALLOCATE(grid%om_s,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8053,&
'frame/module_domain.f: Failed to deallocate grid%om_s. ')
 endif
  NULLIFY(grid%om_s)
ENDIF
IF ( ASSOCIATED( grid%om_depth ) ) THEN 
  DEALLOCATE(grid%om_depth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8061,&
'frame/module_domain.f: Failed to deallocate grid%om_depth. ')
 endif
  NULLIFY(grid%om_depth)
ENDIF
IF ( ASSOCIATED( grid%om_u ) ) THEN 
  DEALLOCATE(grid%om_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8069,&
'frame/module_domain.f: Failed to deallocate grid%om_u. ')
 endif
  NULLIFY(grid%om_u)
ENDIF
IF ( ASSOCIATED( grid%om_v ) ) THEN 
  DEALLOCATE(grid%om_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8077,&
'frame/module_domain.f: Failed to deallocate grid%om_v. ')
 endif
  NULLIFY(grid%om_v)
ENDIF
IF ( ASSOCIATED( grid%om_lat ) ) THEN 
  DEALLOCATE(grid%om_lat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8085,&
'frame/module_domain.f: Failed to deallocate grid%om_lat. ')
 endif
  NULLIFY(grid%om_lat)
ENDIF
IF ( ASSOCIATED( grid%om_lon ) ) THEN 
  DEALLOCATE(grid%om_lon,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8093,&
'frame/module_domain.f: Failed to deallocate grid%om_lon. ')
 endif
  NULLIFY(grid%om_lon)
ENDIF
IF ( ASSOCIATED( grid%om_ml ) ) THEN 
  DEALLOCATE(grid%om_ml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8101,&
'frame/module_domain.f: Failed to deallocate grid%om_ml. ')
 endif
  NULLIFY(grid%om_ml)
ENDIF
IF ( ASSOCIATED( grid%om_tini ) ) THEN 
  DEALLOCATE(grid%om_tini,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8109,&
'frame/module_domain.f: Failed to deallocate grid%om_tini. ')
 endif
  NULLIFY(grid%om_tini)
ENDIF
IF ( ASSOCIATED( grid%om_sini ) ) THEN 
  DEALLOCATE(grid%om_sini,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8117,&
'frame/module_domain.f: Failed to deallocate grid%om_sini. ')
 endif
  NULLIFY(grid%om_sini)
ENDIF
IF ( ASSOCIATED( grid%cupflag ) ) THEN 
  DEALLOCATE(grid%cupflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8125,&
'frame/module_domain.f: Failed to deallocate grid%cupflag. ')
 endif
  NULLIFY(grid%cupflag)
ENDIF
IF ( ASSOCIATED( grid%slopesfc ) ) THEN 
  DEALLOCATE(grid%slopesfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8133,&
'frame/module_domain.f: Failed to deallocate grid%slopesfc. ')
 endif
  NULLIFY(grid%slopesfc)
ENDIF
IF ( ASSOCIATED( grid%slopeez ) ) THEN 
  DEALLOCATE(grid%slopeez,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8141,&
'frame/module_domain.f: Failed to deallocate grid%slopeez. ')
 endif
  NULLIFY(grid%slopeez)
ENDIF
IF ( ASSOCIATED( grid%sigmasfc ) ) THEN 
  DEALLOCATE(grid%sigmasfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8149,&
'frame/module_domain.f: Failed to deallocate grid%sigmasfc. ')
 endif
  NULLIFY(grid%sigmasfc)
ENDIF
IF ( ASSOCIATED( grid%sigmaez ) ) THEN 
  DEALLOCATE(grid%sigmaez,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8157,&
'frame/module_domain.f: Failed to deallocate grid%sigmaez. ')
 endif
  NULLIFY(grid%sigmaez)
ENDIF
IF ( ASSOCIATED( grid%shall ) ) THEN 
  DEALLOCATE(grid%shall,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8165,&
'frame/module_domain.f: Failed to deallocate grid%shall. ')
 endif
  NULLIFY(grid%shall)
ENDIF
IF ( ASSOCIATED( grid%taucloud ) ) THEN 
  DEALLOCATE(grid%taucloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8173,&
'frame/module_domain.f: Failed to deallocate grid%taucloud. ')
 endif
  NULLIFY(grid%taucloud)
ENDIF
IF ( ASSOCIATED( grid%tactive ) ) THEN 
  DEALLOCATE(grid%tactive,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8181,&
'frame/module_domain.f: Failed to deallocate grid%tactive. ')
 endif
  NULLIFY(grid%tactive)
ENDIF
IF ( ASSOCIATED( grid%tcloud_cup ) ) THEN 
  DEALLOCATE(grid%tcloud_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8189,&
'frame/module_domain.f: Failed to deallocate grid%tcloud_cup. ')
 endif
  NULLIFY(grid%tcloud_cup)
ENDIF
IF ( ASSOCIATED( grid%wcloudbase ) ) THEN 
  DEALLOCATE(grid%wcloudbase,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8197,&
'frame/module_domain.f: Failed to deallocate grid%wcloudbase. ')
 endif
  NULLIFY(grid%wcloudbase)
ENDIF
IF ( ASSOCIATED( grid%activefrac ) ) THEN 
  DEALLOCATE(grid%activefrac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8205,&
'frame/module_domain.f: Failed to deallocate grid%activefrac. ')
 endif
  NULLIFY(grid%activefrac)
ENDIF
IF ( ASSOCIATED( grid%cldfratend_cup ) ) THEN 
  DEALLOCATE(grid%cldfratend_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8213,&
'frame/module_domain.f: Failed to deallocate grid%cldfratend_cup. ')
 endif
  NULLIFY(grid%cldfratend_cup)
ENDIF
IF ( ASSOCIATED( grid%cldfra_cup ) ) THEN 
  DEALLOCATE(grid%cldfra_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8221,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_cup. ')
 endif
  NULLIFY(grid%cldfra_cup)
ENDIF
IF ( ASSOCIATED( grid%updfra_cup ) ) THEN 
  DEALLOCATE(grid%updfra_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8229,&
'frame/module_domain.f: Failed to deallocate grid%updfra_cup. ')
 endif
  NULLIFY(grid%updfra_cup)
ENDIF
IF ( ASSOCIATED( grid%qc_iu_cup ) ) THEN 
  DEALLOCATE(grid%qc_iu_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8237,&
'frame/module_domain.f: Failed to deallocate grid%qc_iu_cup. ')
 endif
  NULLIFY(grid%qc_iu_cup)
ENDIF
IF ( ASSOCIATED( grid%qc_ic_cup ) ) THEN 
  DEALLOCATE(grid%qc_ic_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8245,&
'frame/module_domain.f: Failed to deallocate grid%qc_ic_cup. ')
 endif
  NULLIFY(grid%qc_ic_cup)
ENDIF
IF ( ASSOCIATED( grid%qndrop_ic_cup ) ) THEN 
  DEALLOCATE(grid%qndrop_ic_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8253,&
'frame/module_domain.f: Failed to deallocate grid%qndrop_ic_cup. ')
 endif
  NULLIFY(grid%qndrop_ic_cup)
ENDIF
IF ( ASSOCIATED( grid%wup_cup ) ) THEN 
  DEALLOCATE(grid%wup_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8261,&
'frame/module_domain.f: Failed to deallocate grid%wup_cup. ')
 endif
  NULLIFY(grid%wup_cup)
ENDIF
IF ( ASSOCIATED( grid%wact_cup ) ) THEN 
  DEALLOCATE(grid%wact_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8269,&
'frame/module_domain.f: Failed to deallocate grid%wact_cup. ')
 endif
  NULLIFY(grid%wact_cup)
ENDIF
IF ( ASSOCIATED( grid%wulcl_cup ) ) THEN 
  DEALLOCATE(grid%wulcl_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8277,&
'frame/module_domain.f: Failed to deallocate grid%wulcl_cup. ')
 endif
  NULLIFY(grid%wulcl_cup)
ENDIF
IF ( ASSOCIATED( grid%mfup_cup ) ) THEN 
  DEALLOCATE(grid%mfup_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8285,&
'frame/module_domain.f: Failed to deallocate grid%mfup_cup. ')
 endif
  NULLIFY(grid%mfup_cup)
ENDIF
IF ( ASSOCIATED( grid%mfup_ent_cup ) ) THEN 
  DEALLOCATE(grid%mfup_ent_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8293,&
'frame/module_domain.f: Failed to deallocate grid%mfup_ent_cup. ')
 endif
  NULLIFY(grid%mfup_ent_cup)
ENDIF
IF ( ASSOCIATED( grid%mfdn_cup ) ) THEN 
  DEALLOCATE(grid%mfdn_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8301,&
'frame/module_domain.f: Failed to deallocate grid%mfdn_cup. ')
 endif
  NULLIFY(grid%mfdn_cup)
ENDIF
IF ( ASSOCIATED( grid%mfdn_ent_cup ) ) THEN 
  DEALLOCATE(grid%mfdn_ent_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8309,&
'frame/module_domain.f: Failed to deallocate grid%mfdn_ent_cup. ')
 endif
  NULLIFY(grid%mfdn_ent_cup)
ENDIF
IF ( ASSOCIATED( grid%fcvt_qc_to_pr_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qc_to_pr_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8317,&
'frame/module_domain.f: Failed to deallocate grid%fcvt_qc_to_pr_cup. ')
 endif
  NULLIFY(grid%fcvt_qc_to_pr_cup)
ENDIF
IF ( ASSOCIATED( grid%fcvt_qc_to_qi_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qc_to_qi_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8325,&
'frame/module_domain.f: Failed to deallocate grid%fcvt_qc_to_qi_cup. ')
 endif
  NULLIFY(grid%fcvt_qc_to_qi_cup)
ENDIF
IF ( ASSOCIATED( grid%fcvt_qi_to_pr_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qi_to_pr_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8333,&
'frame/module_domain.f: Failed to deallocate grid%fcvt_qi_to_pr_cup. ')
 endif
  NULLIFY(grid%fcvt_qi_to_pr_cup)
ENDIF
IF ( ASSOCIATED( grid%tstar ) ) THEN 
  DEALLOCATE(grid%tstar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8341,&
'frame/module_domain.f: Failed to deallocate grid%tstar. ')
 endif
  NULLIFY(grid%tstar)
ENDIF
IF ( ASSOCIATED( grid%lnterms ) ) THEN 
  DEALLOCATE(grid%lnterms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8349,&
'frame/module_domain.f: Failed to deallocate grid%lnterms. ')
 endif
  NULLIFY(grid%lnterms)
ENDIF
IF ( ASSOCIATED( grid%lnint ) ) THEN 
  DEALLOCATE(grid%lnint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8357,&
'frame/module_domain.f: Failed to deallocate grid%lnint. ')
 endif
  NULLIFY(grid%lnint)
ENDIF
IF ( ASSOCIATED( grid%h_diabatic ) ) THEN 
  DEALLOCATE(grid%h_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8365,&
'frame/module_domain.f: Failed to deallocate grid%h_diabatic. ')
 endif
  NULLIFY(grid%h_diabatic)
ENDIF
IF ( ASSOCIATED( grid%qv_diabatic ) ) THEN 
  DEALLOCATE(grid%qv_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8373,&
'frame/module_domain.f: Failed to deallocate grid%qv_diabatic. ')
 endif
  NULLIFY(grid%qv_diabatic)
ENDIF
IF ( ASSOCIATED( grid%qc_diabatic ) ) THEN 
  DEALLOCATE(grid%qc_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8381,&
'frame/module_domain.f: Failed to deallocate grid%qc_diabatic. ')
 endif
  NULLIFY(grid%qc_diabatic)
ENDIF
IF ( ASSOCIATED( grid%msft ) ) THEN 
  DEALLOCATE(grid%msft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8389,&
'frame/module_domain.f: Failed to deallocate grid%msft. ')
 endif
  NULLIFY(grid%msft)
ENDIF
IF ( ASSOCIATED( grid%msfu ) ) THEN 
  DEALLOCATE(grid%msfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8397,&
'frame/module_domain.f: Failed to deallocate grid%msfu. ')
 endif
  NULLIFY(grid%msfu)
ENDIF
IF ( ASSOCIATED( grid%msfv ) ) THEN 
  DEALLOCATE(grid%msfv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8405,&
'frame/module_domain.f: Failed to deallocate grid%msfv. ')
 endif
  NULLIFY(grid%msfv)
ENDIF
IF ( ASSOCIATED( grid%msftx ) ) THEN 
  DEALLOCATE(grid%msftx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8413,&
'frame/module_domain.f: Failed to deallocate grid%msftx. ')
 endif
  NULLIFY(grid%msftx)
ENDIF
IF ( ASSOCIATED( grid%msfty ) ) THEN 
  DEALLOCATE(grid%msfty,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8421,&
'frame/module_domain.f: Failed to deallocate grid%msfty. ')
 endif
  NULLIFY(grid%msfty)
ENDIF
IF ( ASSOCIATED( grid%msfux ) ) THEN 
  DEALLOCATE(grid%msfux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8429,&
'frame/module_domain.f: Failed to deallocate grid%msfux. ')
 endif
  NULLIFY(grid%msfux)
ENDIF
IF ( ASSOCIATED( grid%msfuy ) ) THEN 
  DEALLOCATE(grid%msfuy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8437,&
'frame/module_domain.f: Failed to deallocate grid%msfuy. ')
 endif
  NULLIFY(grid%msfuy)
ENDIF
IF ( ASSOCIATED( grid%msfvx ) ) THEN 
  DEALLOCATE(grid%msfvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8445,&
'frame/module_domain.f: Failed to deallocate grid%msfvx. ')
 endif
  NULLIFY(grid%msfvx)
ENDIF
IF ( ASSOCIATED( grid%msfvx_inv ) ) THEN 
  DEALLOCATE(grid%msfvx_inv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8453,&
'frame/module_domain.f: Failed to deallocate grid%msfvx_inv. ')
 endif
  NULLIFY(grid%msfvx_inv)
ENDIF
IF ( ASSOCIATED( grid%msfvy ) ) THEN 
  DEALLOCATE(grid%msfvy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8461,&
'frame/module_domain.f: Failed to deallocate grid%msfvy. ')
 endif
  NULLIFY(grid%msfvy)
ENDIF
IF ( ASSOCIATED( grid%f ) ) THEN 
  DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8469,&
'frame/module_domain.f: Failed to deallocate grid%f. ')
 endif
  NULLIFY(grid%f)
ENDIF
IF ( ASSOCIATED( grid%e ) ) THEN 
  DEALLOCATE(grid%e,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8477,&
'frame/module_domain.f: Failed to deallocate grid%e. ')
 endif
  NULLIFY(grid%e)
ENDIF
IF ( ASSOCIATED( grid%sina ) ) THEN 
  DEALLOCATE(grid%sina,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8485,&
'frame/module_domain.f: Failed to deallocate grid%sina. ')
 endif
  NULLIFY(grid%sina)
ENDIF
IF ( ASSOCIATED( grid%cosa ) ) THEN 
  DEALLOCATE(grid%cosa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8493,&
'frame/module_domain.f: Failed to deallocate grid%cosa. ')
 endif
  NULLIFY(grid%cosa)
ENDIF
IF ( ASSOCIATED( grid%ht ) ) THEN 
  DEALLOCATE(grid%ht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8501,&
'frame/module_domain.f: Failed to deallocate grid%ht. ')
 endif
  NULLIFY(grid%ht)
ENDIF
IF ( ASSOCIATED( grid%ht_fine ) ) THEN 
  DEALLOCATE(grid%ht_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8509,&
'frame/module_domain.f: Failed to deallocate grid%ht_fine. ')
 endif
  NULLIFY(grid%ht_fine)
ENDIF
IF ( ASSOCIATED( grid%ht_int ) ) THEN 
  DEALLOCATE(grid%ht_int,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8517,&
'frame/module_domain.f: Failed to deallocate grid%ht_int. ')
 endif
  NULLIFY(grid%ht_int)
ENDIF
IF ( ASSOCIATED( grid%ht_input ) ) THEN 
  DEALLOCATE(grid%ht_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8525,&
'frame/module_domain.f: Failed to deallocate grid%ht_input. ')
 endif
  NULLIFY(grid%ht_input)
ENDIF
IF ( ASSOCIATED( grid%ht_smooth ) ) THEN 
  DEALLOCATE(grid%ht_smooth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8533,&
'frame/module_domain.f: Failed to deallocate grid%ht_smooth. ')
 endif
  NULLIFY(grid%ht_smooth)
ENDIF
IF ( ASSOCIATED( grid%ht_shad ) ) THEN 
  DEALLOCATE(grid%ht_shad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8541,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad. ')
 endif
  NULLIFY(grid%ht_shad)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bxs ) ) THEN 
  DEALLOCATE(grid%ht_shad_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8549,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bxs. ')
 endif
  NULLIFY(grid%ht_shad_bxs)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bxe ) ) THEN 
  DEALLOCATE(grid%ht_shad_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8557,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bxe. ')
 endif
  NULLIFY(grid%ht_shad_bxe)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bys ) ) THEN 
  DEALLOCATE(grid%ht_shad_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8565,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bys. ')
 endif
  NULLIFY(grid%ht_shad_bys)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bye ) ) THEN 
  DEALLOCATE(grid%ht_shad_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8573,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bye. ')
 endif
  NULLIFY(grid%ht_shad_bye)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btxs ) ) THEN 
  DEALLOCATE(grid%ht_shad_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8581,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btxs. ')
 endif
  NULLIFY(grid%ht_shad_btxs)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btxe ) ) THEN 
  DEALLOCATE(grid%ht_shad_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8589,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btxe. ')
 endif
  NULLIFY(grid%ht_shad_btxe)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btys ) ) THEN 
  DEALLOCATE(grid%ht_shad_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8597,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btys. ')
 endif
  NULLIFY(grid%ht_shad_btys)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btye ) ) THEN 
  DEALLOCATE(grid%ht_shad_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8605,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btye. ')
 endif
  NULLIFY(grid%ht_shad_btye)
ENDIF
IF ( ASSOCIATED( grid%shadowmask ) ) THEN 
  DEALLOCATE(grid%shadowmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8613,&
'frame/module_domain.f: Failed to deallocate grid%shadowmask. ')
 endif
  NULLIFY(grid%shadowmask)
ENDIF
IF ( ASSOCIATED( grid%tsk ) ) THEN 
  DEALLOCATE(grid%tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8621,&
'frame/module_domain.f: Failed to deallocate grid%tsk. ')
 endif
  NULLIFY(grid%tsk)
ENDIF
IF ( ASSOCIATED( grid%dfi_tsk ) ) THEN 
  DEALLOCATE(grid%dfi_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8629,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tsk. ')
 endif
  NULLIFY(grid%dfi_tsk)
ENDIF
IF ( ASSOCIATED( grid%tsk_save ) ) THEN 
  DEALLOCATE(grid%tsk_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8637,&
'frame/module_domain.f: Failed to deallocate grid%tsk_save. ')
 endif
  NULLIFY(grid%tsk_save)
ENDIF
IF ( ASSOCIATED( grid%u_base ) ) THEN 
  DEALLOCATE(grid%u_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8645,&
'frame/module_domain.f: Failed to deallocate grid%u_base. ')
 endif
  NULLIFY(grid%u_base)
ENDIF
IF ( ASSOCIATED( grid%v_base ) ) THEN 
  DEALLOCATE(grid%v_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8653,&
'frame/module_domain.f: Failed to deallocate grid%v_base. ')
 endif
  NULLIFY(grid%v_base)
ENDIF
IF ( ASSOCIATED( grid%qv_base ) ) THEN 
  DEALLOCATE(grid%qv_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8661,&
'frame/module_domain.f: Failed to deallocate grid%qv_base. ')
 endif
  NULLIFY(grid%qv_base)
ENDIF
IF ( ASSOCIATED( grid%z_base ) ) THEN 
  DEALLOCATE(grid%z_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8669,&
'frame/module_domain.f: Failed to deallocate grid%z_base. ')
 endif
  NULLIFY(grid%z_base)
ENDIF
IF ( ASSOCIATED( grid%phys_tot ) ) THEN 
  DEALLOCATE(grid%phys_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8677,&
'frame/module_domain.f: Failed to deallocate grid%phys_tot. ')
 endif
  NULLIFY(grid%phys_tot)
ENDIF
IF ( ASSOCIATED( grid%physc ) ) THEN 
  DEALLOCATE(grid%physc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8685,&
'frame/module_domain.f: Failed to deallocate grid%physc. ')
 endif
  NULLIFY(grid%physc)
ENDIF
IF ( ASSOCIATED( grid%physe ) ) THEN 
  DEALLOCATE(grid%physe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8693,&
'frame/module_domain.f: Failed to deallocate grid%physe. ')
 endif
  NULLIFY(grid%physe)
ENDIF
IF ( ASSOCIATED( grid%physd ) ) THEN 
  DEALLOCATE(grid%physd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8701,&
'frame/module_domain.f: Failed to deallocate grid%physd. ')
 endif
  NULLIFY(grid%physd)
ENDIF
IF ( ASSOCIATED( grid%physs ) ) THEN 
  DEALLOCATE(grid%physs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8709,&
'frame/module_domain.f: Failed to deallocate grid%physs. ')
 endif
  NULLIFY(grid%physs)
ENDIF
IF ( ASSOCIATED( grid%physm ) ) THEN 
  DEALLOCATE(grid%physm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8717,&
'frame/module_domain.f: Failed to deallocate grid%physm. ')
 endif
  NULLIFY(grid%physm)
ENDIF
IF ( ASSOCIATED( grid%physf ) ) THEN 
  DEALLOCATE(grid%physf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8725,&
'frame/module_domain.f: Failed to deallocate grid%physf. ')
 endif
  NULLIFY(grid%physf)
ENDIF
IF ( ASSOCIATED( grid%acphys_tot ) ) THEN 
  DEALLOCATE(grid%acphys_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8733,&
'frame/module_domain.f: Failed to deallocate grid%acphys_tot. ')
 endif
  NULLIFY(grid%acphys_tot)
ENDIF
IF ( ASSOCIATED( grid%acphysc ) ) THEN 
  DEALLOCATE(grid%acphysc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8741,&
'frame/module_domain.f: Failed to deallocate grid%acphysc. ')
 endif
  NULLIFY(grid%acphysc)
ENDIF
IF ( ASSOCIATED( grid%acphyse ) ) THEN 
  DEALLOCATE(grid%acphyse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8749,&
'frame/module_domain.f: Failed to deallocate grid%acphyse. ')
 endif
  NULLIFY(grid%acphyse)
ENDIF
IF ( ASSOCIATED( grid%acphysd ) ) THEN 
  DEALLOCATE(grid%acphysd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8757,&
'frame/module_domain.f: Failed to deallocate grid%acphysd. ')
 endif
  NULLIFY(grid%acphysd)
ENDIF
IF ( ASSOCIATED( grid%acphyss ) ) THEN 
  DEALLOCATE(grid%acphyss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8765,&
'frame/module_domain.f: Failed to deallocate grid%acphyss. ')
 endif
  NULLIFY(grid%acphyss)
ENDIF
IF ( ASSOCIATED( grid%acphysm ) ) THEN 
  DEALLOCATE(grid%acphysm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8773,&
'frame/module_domain.f: Failed to deallocate grid%acphysm. ')
 endif
  NULLIFY(grid%acphysm)
ENDIF
IF ( ASSOCIATED( grid%acphysf ) ) THEN 
  DEALLOCATE(grid%acphysf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8781,&
'frame/module_domain.f: Failed to deallocate grid%acphysf. ')
 endif
  NULLIFY(grid%acphysf)
ENDIF
IF ( ASSOCIATED( grid%preci3d ) ) THEN 
  DEALLOCATE(grid%preci3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8789,&
'frame/module_domain.f: Failed to deallocate grid%preci3d. ')
 endif
  NULLIFY(grid%preci3d)
ENDIF
IF ( ASSOCIATED( grid%precs3d ) ) THEN 
  DEALLOCATE(grid%precs3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8797,&
'frame/module_domain.f: Failed to deallocate grid%precs3d. ')
 endif
  NULLIFY(grid%precs3d)
ENDIF
IF ( ASSOCIATED( grid%precg3d ) ) THEN 
  DEALLOCATE(grid%precg3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8805,&
'frame/module_domain.f: Failed to deallocate grid%precg3d. ')
 endif
  NULLIFY(grid%precg3d)
ENDIF
IF ( ASSOCIATED( grid%prech3d ) ) THEN 
  DEALLOCATE(grid%prech3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8813,&
'frame/module_domain.f: Failed to deallocate grid%prech3d. ')
 endif
  NULLIFY(grid%prech3d)
ENDIF
IF ( ASSOCIATED( grid%precr3d ) ) THEN 
  DEALLOCATE(grid%precr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8821,&
'frame/module_domain.f: Failed to deallocate grid%precr3d. ')
 endif
  NULLIFY(grid%precr3d)
ENDIF
IF ( ASSOCIATED( grid%tlwdn ) ) THEN 
  DEALLOCATE(grid%tlwdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8829,&
'frame/module_domain.f: Failed to deallocate grid%tlwdn. ')
 endif
  NULLIFY(grid%tlwdn)
ENDIF
IF ( ASSOCIATED( grid%tlwup ) ) THEN 
  DEALLOCATE(grid%tlwup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8837,&
'frame/module_domain.f: Failed to deallocate grid%tlwup. ')
 endif
  NULLIFY(grid%tlwup)
ENDIF
IF ( ASSOCIATED( grid%slwdn ) ) THEN 
  DEALLOCATE(grid%slwdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8845,&
'frame/module_domain.f: Failed to deallocate grid%slwdn. ')
 endif
  NULLIFY(grid%slwdn)
ENDIF
IF ( ASSOCIATED( grid%slwup ) ) THEN 
  DEALLOCATE(grid%slwup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8853,&
'frame/module_domain.f: Failed to deallocate grid%slwup. ')
 endif
  NULLIFY(grid%slwup)
ENDIF
IF ( ASSOCIATED( grid%tswdn ) ) THEN 
  DEALLOCATE(grid%tswdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8861,&
'frame/module_domain.f: Failed to deallocate grid%tswdn. ')
 endif
  NULLIFY(grid%tswdn)
ENDIF
IF ( ASSOCIATED( grid%tswup ) ) THEN 
  DEALLOCATE(grid%tswup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8869,&
'frame/module_domain.f: Failed to deallocate grid%tswup. ')
 endif
  NULLIFY(grid%tswup)
ENDIF
IF ( ASSOCIATED( grid%sswdn ) ) THEN 
  DEALLOCATE(grid%sswdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8877,&
'frame/module_domain.f: Failed to deallocate grid%sswdn. ')
 endif
  NULLIFY(grid%sswdn)
ENDIF
IF ( ASSOCIATED( grid%sswup ) ) THEN 
  DEALLOCATE(grid%sswup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8885,&
'frame/module_domain.f: Failed to deallocate grid%sswup. ')
 endif
  NULLIFY(grid%sswup)
ENDIF
IF ( ASSOCIATED( grid%cod2d_out ) ) THEN 
  DEALLOCATE(grid%cod2d_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8893,&
'frame/module_domain.f: Failed to deallocate grid%cod2d_out. ')
 endif
  NULLIFY(grid%cod2d_out)
ENDIF
IF ( ASSOCIATED( grid%ctop2d_out ) ) THEN 
  DEALLOCATE(grid%ctop2d_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8901,&
'frame/module_domain.f: Failed to deallocate grid%ctop2d_out. ')
 endif
  NULLIFY(grid%ctop2d_out)
ENDIF
IF ( ASSOCIATED( grid%rushten ) ) THEN 
  DEALLOCATE(grid%rushten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8909,&
'frame/module_domain.f: Failed to deallocate grid%rushten. ')
 endif
  NULLIFY(grid%rushten)
ENDIF
IF ( ASSOCIATED( grid%rvshten ) ) THEN 
  DEALLOCATE(grid%rvshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8917,&
'frame/module_domain.f: Failed to deallocate grid%rvshten. ')
 endif
  NULLIFY(grid%rvshten)
ENDIF
IF ( ASSOCIATED( grid%rthshten ) ) THEN 
  DEALLOCATE(grid%rthshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8925,&
'frame/module_domain.f: Failed to deallocate grid%rthshten. ')
 endif
  NULLIFY(grid%rthshten)
ENDIF
IF ( ASSOCIATED( grid%rqvshten ) ) THEN 
  DEALLOCATE(grid%rqvshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8933,&
'frame/module_domain.f: Failed to deallocate grid%rqvshten. ')
 endif
  NULLIFY(grid%rqvshten)
ENDIF
IF ( ASSOCIATED( grid%rqrshten ) ) THEN 
  DEALLOCATE(grid%rqrshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8941,&
'frame/module_domain.f: Failed to deallocate grid%rqrshten. ')
 endif
  NULLIFY(grid%rqrshten)
ENDIF
IF ( ASSOCIATED( grid%rqcshten ) ) THEN 
  DEALLOCATE(grid%rqcshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8949,&
'frame/module_domain.f: Failed to deallocate grid%rqcshten. ')
 endif
  NULLIFY(grid%rqcshten)
ENDIF
IF ( ASSOCIATED( grid%rqsshten ) ) THEN 
  DEALLOCATE(grid%rqsshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8957,&
'frame/module_domain.f: Failed to deallocate grid%rqsshten. ')
 endif
  NULLIFY(grid%rqsshten)
ENDIF
IF ( ASSOCIATED( grid%rqishten ) ) THEN 
  DEALLOCATE(grid%rqishten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8965,&
'frame/module_domain.f: Failed to deallocate grid%rqishten. ')
 endif
  NULLIFY(grid%rqishten)
ENDIF
IF ( ASSOCIATED( grid%rqgshten ) ) THEN 
  DEALLOCATE(grid%rqgshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8973,&
'frame/module_domain.f: Failed to deallocate grid%rqgshten. ')
 endif
  NULLIFY(grid%rqgshten)
ENDIF
IF ( ASSOCIATED( grid%rqcnshten ) ) THEN 
  DEALLOCATE(grid%rqcnshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8981,&
'frame/module_domain.f: Failed to deallocate grid%rqcnshten. ')
 endif
  NULLIFY(grid%rqcnshten)
ENDIF
IF ( ASSOCIATED( grid%rqinshten ) ) THEN 
  DEALLOCATE(grid%rqinshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8989,&
'frame/module_domain.f: Failed to deallocate grid%rqinshten. ')
 endif
  NULLIFY(grid%rqinshten)
ENDIF
IF ( ASSOCIATED( grid%rdcashten ) ) THEN 
  DEALLOCATE(grid%rdcashten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8997,&
'frame/module_domain.f: Failed to deallocate grid%rdcashten. ')
 endif
  NULLIFY(grid%rdcashten)
ENDIF
IF ( ASSOCIATED( grid%rqcdcshten ) ) THEN 
  DEALLOCATE(grid%rqcdcshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9005,&
'frame/module_domain.f: Failed to deallocate grid%rqcdcshten. ')
 endif
  NULLIFY(grid%rqcdcshten)
ENDIF
IF ( ASSOCIATED( grid%cldareaa ) ) THEN 
  DEALLOCATE(grid%cldareaa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9013,&
'frame/module_domain.f: Failed to deallocate grid%cldareaa. ')
 endif
  NULLIFY(grid%cldareaa)
ENDIF
IF ( ASSOCIATED( grid%cldareab ) ) THEN 
  DEALLOCATE(grid%cldareab,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9021,&
'frame/module_domain.f: Failed to deallocate grid%cldareab. ')
 endif
  NULLIFY(grid%cldareab)
ENDIF
IF ( ASSOCIATED( grid%ca_rad ) ) THEN 
  DEALLOCATE(grid%ca_rad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9029,&
'frame/module_domain.f: Failed to deallocate grid%ca_rad. ')
 endif
  NULLIFY(grid%ca_rad)
ENDIF
IF ( ASSOCIATED( grid%cw_rad ) ) THEN 
  DEALLOCATE(grid%cw_rad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9037,&
'frame/module_domain.f: Failed to deallocate grid%cw_rad. ')
 endif
  NULLIFY(grid%cw_rad)
ENDIF
IF ( ASSOCIATED( grid%cldliqa ) ) THEN 
  DEALLOCATE(grid%cldliqa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9045,&
'frame/module_domain.f: Failed to deallocate grid%cldliqa. ')
 endif
  NULLIFY(grid%cldliqa)
ENDIF
IF ( ASSOCIATED( grid%cldliqb ) ) THEN 
  DEALLOCATE(grid%cldliqb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9053,&
'frame/module_domain.f: Failed to deallocate grid%cldliqb. ')
 endif
  NULLIFY(grid%cldliqb)
ENDIF
IF ( ASSOCIATED( grid%clddpthb ) ) THEN 
  DEALLOCATE(grid%clddpthb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9061,&
'frame/module_domain.f: Failed to deallocate grid%clddpthb. ')
 endif
  NULLIFY(grid%clddpthb)
ENDIF
IF ( ASSOCIATED( grid%cldtopb ) ) THEN 
  DEALLOCATE(grid%cldtopb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9069,&
'frame/module_domain.f: Failed to deallocate grid%cldtopb. ')
 endif
  NULLIFY(grid%cldtopb)
ENDIF
IF ( ASSOCIATED( grid%pblmax ) ) THEN 
  DEALLOCATE(grid%pblmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9077,&
'frame/module_domain.f: Failed to deallocate grid%pblmax. ')
 endif
  NULLIFY(grid%pblmax)
ENDIF
IF ( ASSOCIATED( grid%wub ) ) THEN 
  DEALLOCATE(grid%wub,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9085,&
'frame/module_domain.f: Failed to deallocate grid%wub. ')
 endif
  NULLIFY(grid%wub)
ENDIF
IF ( ASSOCIATED( grid%rainshvb ) ) THEN 
  DEALLOCATE(grid%rainshvb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9093,&
'frame/module_domain.f: Failed to deallocate grid%rainshvb. ')
 endif
  NULLIFY(grid%rainshvb)
ENDIF
IF ( ASSOCIATED( grid%capesave ) ) THEN 
  DEALLOCATE(grid%capesave,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9101,&
'frame/module_domain.f: Failed to deallocate grid%capesave. ')
 endif
  NULLIFY(grid%capesave)
ENDIF
IF ( ASSOCIATED( grid%radsave ) ) THEN 
  DEALLOCATE(grid%radsave,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9109,&
'frame/module_domain.f: Failed to deallocate grid%radsave. ')
 endif
  NULLIFY(grid%radsave)
ENDIF
IF ( ASSOCIATED( grid%ainckfsa ) ) THEN 
  DEALLOCATE(grid%ainckfsa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9117,&
'frame/module_domain.f: Failed to deallocate grid%ainckfsa. ')
 endif
  NULLIFY(grid%ainckfsa)
ENDIF
IF ( ASSOCIATED( grid%ltopb ) ) THEN 
  DEALLOCATE(grid%ltopb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9125,&
'frame/module_domain.f: Failed to deallocate grid%ltopb. ')
 endif
  NULLIFY(grid%ltopb)
ENDIF
IF ( ASSOCIATED( grid%kdcldtop ) ) THEN 
  DEALLOCATE(grid%kdcldtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9133,&
'frame/module_domain.f: Failed to deallocate grid%kdcldtop. ')
 endif
  NULLIFY(grid%kdcldtop)
ENDIF
IF ( ASSOCIATED( grid%kdcldbas ) ) THEN 
  DEALLOCATE(grid%kdcldbas,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9141,&
'frame/module_domain.f: Failed to deallocate grid%kdcldbas. ')
 endif
  NULLIFY(grid%kdcldbas)
ENDIF
IF ( ASSOCIATED( grid%xtime1 ) ) THEN 
  DEALLOCATE(grid%xtime1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9149,&
'frame/module_domain.f: Failed to deallocate grid%xtime1. ')
 endif
  NULLIFY(grid%xtime1)
ENDIF
IF ( ASSOCIATED( grid%pblhavg ) ) THEN 
  DEALLOCATE(grid%pblhavg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9157,&
'frame/module_domain.f: Failed to deallocate grid%pblhavg. ')
 endif
  NULLIFY(grid%pblhavg)
ENDIF
IF ( ASSOCIATED( grid%tkeavg ) ) THEN 
  DEALLOCATE(grid%tkeavg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9165,&
'frame/module_domain.f: Failed to deallocate grid%tkeavg. ')
 endif
  NULLIFY(grid%tkeavg)
ENDIF
IF ( ASSOCIATED( grid%wsubsid ) ) THEN 
  DEALLOCATE(grid%wsubsid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9173,&
'frame/module_domain.f: Failed to deallocate grid%wsubsid. ')
 endif
  NULLIFY(grid%wsubsid)
ENDIF
IF ( ASSOCIATED( grid%rucuten ) ) THEN 
  DEALLOCATE(grid%rucuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9181,&
'frame/module_domain.f: Failed to deallocate grid%rucuten. ')
 endif
  NULLIFY(grid%rucuten)
ENDIF
IF ( ASSOCIATED( grid%rvcuten ) ) THEN 
  DEALLOCATE(grid%rvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9189,&
'frame/module_domain.f: Failed to deallocate grid%rvcuten. ')
 endif
  NULLIFY(grid%rvcuten)
ENDIF
IF ( ASSOCIATED( grid%rthcuten ) ) THEN 
  DEALLOCATE(grid%rthcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9197,&
'frame/module_domain.f: Failed to deallocate grid%rthcuten. ')
 endif
  NULLIFY(grid%rthcuten)
ENDIF
IF ( ASSOCIATED( grid%rqvcuten ) ) THEN 
  DEALLOCATE(grid%rqvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9205,&
'frame/module_domain.f: Failed to deallocate grid%rqvcuten. ')
 endif
  NULLIFY(grid%rqvcuten)
ENDIF
IF ( ASSOCIATED( grid%rqrcuten ) ) THEN 
  DEALLOCATE(grid%rqrcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9213,&
'frame/module_domain.f: Failed to deallocate grid%rqrcuten. ')
 endif
  NULLIFY(grid%rqrcuten)
ENDIF
IF ( ASSOCIATED( grid%rqccuten ) ) THEN 
  DEALLOCATE(grid%rqccuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9221,&
'frame/module_domain.f: Failed to deallocate grid%rqccuten. ')
 endif
  NULLIFY(grid%rqccuten)
ENDIF
IF ( ASSOCIATED( grid%rqscuten ) ) THEN 
  DEALLOCATE(grid%rqscuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9229,&
'frame/module_domain.f: Failed to deallocate grid%rqscuten. ')
 endif
  NULLIFY(grid%rqscuten)
ENDIF
IF ( ASSOCIATED( grid%rqicuten ) ) THEN 
  DEALLOCATE(grid%rqicuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9237,&
'frame/module_domain.f: Failed to deallocate grid%rqicuten. ')
 endif
  NULLIFY(grid%rqicuten)
ENDIF
IF ( ASSOCIATED( grid%rqcncuten ) ) THEN 
  DEALLOCATE(grid%rqcncuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9245,&
'frame/module_domain.f: Failed to deallocate grid%rqcncuten. ')
 endif
  NULLIFY(grid%rqcncuten)
ENDIF
IF ( ASSOCIATED( grid%rqincuten ) ) THEN 
  DEALLOCATE(grid%rqincuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9253,&
'frame/module_domain.f: Failed to deallocate grid%rqincuten. ')
 endif
  NULLIFY(grid%rqincuten)
ENDIF
IF ( ASSOCIATED( grid%w0avg ) ) THEN 
  DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9261,&
'frame/module_domain.f: Failed to deallocate grid%w0avg. ')
 endif
  NULLIFY(grid%w0avg)
ENDIF
IF ( ASSOCIATED( grid%qcconv ) ) THEN 
  DEALLOCATE(grid%qcconv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9269,&
'frame/module_domain.f: Failed to deallocate grid%qcconv. ')
 endif
  NULLIFY(grid%qcconv)
ENDIF
IF ( ASSOCIATED( grid%qiconv ) ) THEN 
  DEALLOCATE(grid%qiconv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9277,&
'frame/module_domain.f: Failed to deallocate grid%qiconv. ')
 endif
  NULLIFY(grid%qiconv)
ENDIF
IF ( ASSOCIATED( grid%rainc ) ) THEN 
  DEALLOCATE(grid%rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9285,&
'frame/module_domain.f: Failed to deallocate grid%rainc. ')
 endif
  NULLIFY(grid%rainc)
ENDIF
IF ( ASSOCIATED( grid%rainsh ) ) THEN 
  DEALLOCATE(grid%rainsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9293,&
'frame/module_domain.f: Failed to deallocate grid%rainsh. ')
 endif
  NULLIFY(grid%rainsh)
ENDIF
IF ( ASSOCIATED( grid%rainnc ) ) THEN 
  DEALLOCATE(grid%rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9301,&
'frame/module_domain.f: Failed to deallocate grid%rainnc. ')
 endif
  NULLIFY(grid%rainnc)
ENDIF
IF ( ASSOCIATED( grid%i_rainc ) ) THEN 
  DEALLOCATE(grid%i_rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9309,&
'frame/module_domain.f: Failed to deallocate grid%i_rainc. ')
 endif
  NULLIFY(grid%i_rainc)
ENDIF
IF ( ASSOCIATED( grid%i_rainnc ) ) THEN 
  DEALLOCATE(grid%i_rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9317,&
'frame/module_domain.f: Failed to deallocate grid%i_rainnc. ')
 endif
  NULLIFY(grid%i_rainnc)
ENDIF
IF ( ASSOCIATED( grid%pratec ) ) THEN 
  DEALLOCATE(grid%pratec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9325,&
'frame/module_domain.f: Failed to deallocate grid%pratec. ')
 endif
  NULLIFY(grid%pratec)
ENDIF
IF ( ASSOCIATED( grid%pratesh ) ) THEN 
  DEALLOCATE(grid%pratesh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9333,&
'frame/module_domain.f: Failed to deallocate grid%pratesh. ')
 endif
  NULLIFY(grid%pratesh)
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9341,&
'frame/module_domain.f: Failed to deallocate grid%raincv. ')
 endif
  NULLIFY(grid%raincv)
ENDIF
IF ( ASSOCIATED( grid%rainshv ) ) THEN 
  DEALLOCATE(grid%rainshv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9349,&
'frame/module_domain.f: Failed to deallocate grid%rainshv. ')
 endif
  NULLIFY(grid%rainshv)
ENDIF
IF ( ASSOCIATED( grid%rainncv ) ) THEN 
  DEALLOCATE(grid%rainncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9357,&
'frame/module_domain.f: Failed to deallocate grid%rainncv. ')
 endif
  NULLIFY(grid%rainncv)
ENDIF
IF ( ASSOCIATED( grid%rainbl ) ) THEN 
  DEALLOCATE(grid%rainbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9365,&
'frame/module_domain.f: Failed to deallocate grid%rainbl. ')
 endif
  NULLIFY(grid%rainbl)
ENDIF
IF ( ASSOCIATED( grid%snownc ) ) THEN 
  DEALLOCATE(grid%snownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9373,&
'frame/module_domain.f: Failed to deallocate grid%snownc. ')
 endif
  NULLIFY(grid%snownc)
ENDIF
IF ( ASSOCIATED( grid%graupelnc ) ) THEN 
  DEALLOCATE(grid%graupelnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9381,&
'frame/module_domain.f: Failed to deallocate grid%graupelnc. ')
 endif
  NULLIFY(grid%graupelnc)
ENDIF
IF ( ASSOCIATED( grid%hailnc ) ) THEN 
  DEALLOCATE(grid%hailnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9389,&
'frame/module_domain.f: Failed to deallocate grid%hailnc. ')
 endif
  NULLIFY(grid%hailnc)
ENDIF
IF ( ASSOCIATED( grid%snowncv ) ) THEN 
  DEALLOCATE(grid%snowncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9397,&
'frame/module_domain.f: Failed to deallocate grid%snowncv. ')
 endif
  NULLIFY(grid%snowncv)
ENDIF
IF ( ASSOCIATED( grid%graupelncv ) ) THEN 
  DEALLOCATE(grid%graupelncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9405,&
'frame/module_domain.f: Failed to deallocate grid%graupelncv. ')
 endif
  NULLIFY(grid%graupelncv)
ENDIF
IF ( ASSOCIATED( grid%hailncv ) ) THEN 
  DEALLOCATE(grid%hailncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9413,&
'frame/module_domain.f: Failed to deallocate grid%hailncv. ')
 endif
  NULLIFY(grid%hailncv)
ENDIF
IF ( ASSOCIATED( grid%refl_10cm ) ) THEN 
  DEALLOCATE(grid%refl_10cm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9421,&
'frame/module_domain.f: Failed to deallocate grid%refl_10cm. ')
 endif
  NULLIFY(grid%refl_10cm)
ENDIF
IF ( ASSOCIATED( grid%mskf_refl_10cm ) ) THEN 
  DEALLOCATE(grid%mskf_refl_10cm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9429,&
'frame/module_domain.f: Failed to deallocate grid%mskf_refl_10cm. ')
 endif
  NULLIFY(grid%mskf_refl_10cm)
ENDIF
IF ( ASSOCIATED( grid%th_old ) ) THEN 
  DEALLOCATE(grid%th_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9437,&
'frame/module_domain.f: Failed to deallocate grid%th_old. ')
 endif
  NULLIFY(grid%th_old)
ENDIF
IF ( ASSOCIATED( grid%qv_old ) ) THEN 
  DEALLOCATE(grid%qv_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9445,&
'frame/module_domain.f: Failed to deallocate grid%qv_old. ')
 endif
  NULLIFY(grid%qv_old)
ENDIF
IF ( ASSOCIATED( grid%vmi3d ) ) THEN 
  DEALLOCATE(grid%vmi3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9453,&
'frame/module_domain.f: Failed to deallocate grid%vmi3d. ')
 endif
  NULLIFY(grid%vmi3d)
ENDIF
IF ( ASSOCIATED( grid%di3d ) ) THEN 
  DEALLOCATE(grid%di3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9461,&
'frame/module_domain.f: Failed to deallocate grid%di3d. ')
 endif
  NULLIFY(grid%di3d)
ENDIF
IF ( ASSOCIATED( grid%rhopo3d ) ) THEN 
  DEALLOCATE(grid%rhopo3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9469,&
'frame/module_domain.f: Failed to deallocate grid%rhopo3d. ')
 endif
  NULLIFY(grid%rhopo3d)
ENDIF
IF ( ASSOCIATED( grid%phii3d ) ) THEN 
  DEALLOCATE(grid%phii3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9477,&
'frame/module_domain.f: Failed to deallocate grid%phii3d. ')
 endif
  NULLIFY(grid%phii3d)
ENDIF
IF ( ASSOCIATED( grid%vmi3d_2 ) ) THEN 
  DEALLOCATE(grid%vmi3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9485,&
'frame/module_domain.f: Failed to deallocate grid%vmi3d_2. ')
 endif
  NULLIFY(grid%vmi3d_2)
ENDIF
IF ( ASSOCIATED( grid%di3d_2 ) ) THEN 
  DEALLOCATE(grid%di3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9493,&
'frame/module_domain.f: Failed to deallocate grid%di3d_2. ')
 endif
  NULLIFY(grid%di3d_2)
ENDIF
IF ( ASSOCIATED( grid%rhopo3d_2 ) ) THEN 
  DEALLOCATE(grid%rhopo3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9501,&
'frame/module_domain.f: Failed to deallocate grid%rhopo3d_2. ')
 endif
  NULLIFY(grid%rhopo3d_2)
ENDIF
IF ( ASSOCIATED( grid%phii3d_2 ) ) THEN 
  DEALLOCATE(grid%phii3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9509,&
'frame/module_domain.f: Failed to deallocate grid%phii3d_2. ')
 endif
  NULLIFY(grid%phii3d_2)
ENDIF
IF ( ASSOCIATED( grid%vmi3d_3 ) ) THEN 
  DEALLOCATE(grid%vmi3d_3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9517,&
'frame/module_domain.f: Failed to deallocate grid%vmi3d_3. ')
 endif
  NULLIFY(grid%vmi3d_3)
ENDIF
IF ( ASSOCIATED( grid%di3d_3 ) ) THEN 
  DEALLOCATE(grid%di3d_3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9525,&
'frame/module_domain.f: Failed to deallocate grid%di3d_3. ')
 endif
  NULLIFY(grid%di3d_3)
ENDIF
IF ( ASSOCIATED( grid%rhopo3d_3 ) ) THEN 
  DEALLOCATE(grid%rhopo3d_3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9533,&
'frame/module_domain.f: Failed to deallocate grid%rhopo3d_3. ')
 endif
  NULLIFY(grid%rhopo3d_3)
ENDIF
IF ( ASSOCIATED( grid%phii3d_3 ) ) THEN 
  DEALLOCATE(grid%phii3d_3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9541,&
'frame/module_domain.f: Failed to deallocate grid%phii3d_3. ')
 endif
  NULLIFY(grid%phii3d_3)
ENDIF
IF ( ASSOCIATED( grid%itype ) ) THEN 
  DEALLOCATE(grid%itype,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9549,&
'frame/module_domain.f: Failed to deallocate grid%itype. ')
 endif
  NULLIFY(grid%itype)
ENDIF
IF ( ASSOCIATED( grid%itype_2 ) ) THEN 
  DEALLOCATE(grid%itype_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9557,&
'frame/module_domain.f: Failed to deallocate grid%itype_2. ')
 endif
  NULLIFY(grid%itype_2)
ENDIF
IF ( ASSOCIATED( grid%itype_3 ) ) THEN 
  DEALLOCATE(grid%itype_3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9565,&
'frame/module_domain.f: Failed to deallocate grid%itype_3. ')
 endif
  NULLIFY(grid%itype_3)
ENDIF
IF ( ASSOCIATED( grid%nca ) ) THEN 
  DEALLOCATE(grid%nca,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9573,&
'frame/module_domain.f: Failed to deallocate grid%nca. ')
 endif
  NULLIFY(grid%nca)
ENDIF
IF ( ASSOCIATED( grid%lowlyr ) ) THEN 
  DEALLOCATE(grid%lowlyr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9581,&
'frame/module_domain.f: Failed to deallocate grid%lowlyr. ')
 endif
  NULLIFY(grid%lowlyr)
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9589,&
'frame/module_domain.f: Failed to deallocate grid%mass_flux. ')
 endif
  NULLIFY(grid%mass_flux)
ENDIF
IF ( ASSOCIATED( grid%cldfra_dp ) ) THEN 
  DEALLOCATE(grid%cldfra_dp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9597,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_dp. ')
 endif
  NULLIFY(grid%cldfra_dp)
ENDIF
IF ( ASSOCIATED( grid%cldfra_sh ) ) THEN 
  DEALLOCATE(grid%cldfra_sh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9605,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_sh. ')
 endif
  NULLIFY(grid%cldfra_sh)
ENDIF
IF ( ASSOCIATED( grid%udr_kf ) ) THEN 
  DEALLOCATE(grid%udr_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9613,&
'frame/module_domain.f: Failed to deallocate grid%udr_kf. ')
 endif
  NULLIFY(grid%udr_kf)
ENDIF
IF ( ASSOCIATED( grid%ddr_kf ) ) THEN 
  DEALLOCATE(grid%ddr_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9621,&
'frame/module_domain.f: Failed to deallocate grid%ddr_kf. ')
 endif
  NULLIFY(grid%ddr_kf)
ENDIF
IF ( ASSOCIATED( grid%uer_kf ) ) THEN 
  DEALLOCATE(grid%uer_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9629,&
'frame/module_domain.f: Failed to deallocate grid%uer_kf. ')
 endif
  NULLIFY(grid%uer_kf)
ENDIF
IF ( ASSOCIATED( grid%der_kf ) ) THEN 
  DEALLOCATE(grid%der_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9637,&
'frame/module_domain.f: Failed to deallocate grid%der_kf. ')
 endif
  NULLIFY(grid%der_kf)
ENDIF
IF ( ASSOCIATED( grid%timec_kf ) ) THEN 
  DEALLOCATE(grid%timec_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9645,&
'frame/module_domain.f: Failed to deallocate grid%timec_kf. ')
 endif
  NULLIFY(grid%timec_kf)
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9653,&
'frame/module_domain.f: Failed to deallocate grid%apr_gr. ')
 endif
  NULLIFY(grid%apr_gr)
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9661,&
'frame/module_domain.f: Failed to deallocate grid%apr_w. ')
 endif
  NULLIFY(grid%apr_w)
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9669,&
'frame/module_domain.f: Failed to deallocate grid%apr_mc. ')
 endif
  NULLIFY(grid%apr_mc)
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9677,&
'frame/module_domain.f: Failed to deallocate grid%apr_st. ')
 endif
  NULLIFY(grid%apr_st)
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9685,&
'frame/module_domain.f: Failed to deallocate grid%apr_as. ')
 endif
  NULLIFY(grid%apr_as)
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9693,&
'frame/module_domain.f: Failed to deallocate grid%apr_capma. ')
 endif
  NULLIFY(grid%apr_capma)
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9701,&
'frame/module_domain.f: Failed to deallocate grid%apr_capme. ')
 endif
  NULLIFY(grid%apr_capme)
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9709,&
'frame/module_domain.f: Failed to deallocate grid%apr_capmi. ')
 endif
  NULLIFY(grid%apr_capmi)
ENDIF
IF ( ASSOCIATED( grid%edt_out ) ) THEN 
  DEALLOCATE(grid%edt_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9717,&
'frame/module_domain.f: Failed to deallocate grid%edt_out. ')
 endif
  NULLIFY(grid%edt_out)
ENDIF
IF ( ASSOCIATED( grid%xmb_shallow ) ) THEN 
  DEALLOCATE(grid%xmb_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9725,&
'frame/module_domain.f: Failed to deallocate grid%xmb_shallow. ')
 endif
  NULLIFY(grid%xmb_shallow)
ENDIF
IF ( ASSOCIATED( grid%k22_shallow ) ) THEN 
  DEALLOCATE(grid%k22_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9733,&
'frame/module_domain.f: Failed to deallocate grid%k22_shallow. ')
 endif
  NULLIFY(grid%k22_shallow)
ENDIF
IF ( ASSOCIATED( grid%kbcon_shallow ) ) THEN 
  DEALLOCATE(grid%kbcon_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9741,&
'frame/module_domain.f: Failed to deallocate grid%kbcon_shallow. ')
 endif
  NULLIFY(grid%kbcon_shallow)
ENDIF
IF ( ASSOCIATED( grid%ktop_shallow ) ) THEN 
  DEALLOCATE(grid%ktop_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9749,&
'frame/module_domain.f: Failed to deallocate grid%ktop_shallow. ')
 endif
  NULLIFY(grid%ktop_shallow)
ENDIF
IF ( ASSOCIATED( grid%k22_deep ) ) THEN 
  DEALLOCATE(grid%k22_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9757,&
'frame/module_domain.f: Failed to deallocate grid%k22_deep. ')
 endif
  NULLIFY(grid%k22_deep)
ENDIF
IF ( ASSOCIATED( grid%kbcon_deep ) ) THEN 
  DEALLOCATE(grid%kbcon_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9765,&
'frame/module_domain.f: Failed to deallocate grid%kbcon_deep. ')
 endif
  NULLIFY(grid%kbcon_deep)
ENDIF
IF ( ASSOCIATED( grid%ktop_deep ) ) THEN 
  DEALLOCATE(grid%ktop_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9773,&
'frame/module_domain.f: Failed to deallocate grid%ktop_deep. ')
 endif
  NULLIFY(grid%ktop_deep)
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9781,&
'frame/module_domain.f: Failed to deallocate grid%xf_ens. ')
 endif
  NULLIFY(grid%xf_ens)
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9789,&
'frame/module_domain.f: Failed to deallocate grid%pr_ens. ')
 endif
  NULLIFY(grid%pr_ens)
ENDIF
IF ( ASSOCIATED( grid%cugd_tten ) ) THEN 
  DEALLOCATE(grid%cugd_tten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9797,&
'frame/module_domain.f: Failed to deallocate grid%cugd_tten. ')
 endif
  NULLIFY(grid%cugd_tten)
ENDIF
IF ( ASSOCIATED( grid%cugd_qvten ) ) THEN 
  DEALLOCATE(grid%cugd_qvten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9805,&
'frame/module_domain.f: Failed to deallocate grid%cugd_qvten. ')
 endif
  NULLIFY(grid%cugd_qvten)
ENDIF
IF ( ASSOCIATED( grid%cugd_ttens ) ) THEN 
  DEALLOCATE(grid%cugd_ttens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9813,&
'frame/module_domain.f: Failed to deallocate grid%cugd_ttens. ')
 endif
  NULLIFY(grid%cugd_ttens)
ENDIF
IF ( ASSOCIATED( grid%cugd_qvtens ) ) THEN 
  DEALLOCATE(grid%cugd_qvtens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9821,&
'frame/module_domain.f: Failed to deallocate grid%cugd_qvtens. ')
 endif
  NULLIFY(grid%cugd_qvtens)
ENDIF
IF ( ASSOCIATED( grid%cugd_qcten ) ) THEN 
  DEALLOCATE(grid%cugd_qcten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9829,&
'frame/module_domain.f: Failed to deallocate grid%cugd_qcten. ')
 endif
  NULLIFY(grid%cugd_qcten)
ENDIF
IF ( ASSOCIATED( grid%gd_cloud ) ) THEN 
  DEALLOCATE(grid%gd_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9837,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud. ')
 endif
  NULLIFY(grid%gd_cloud)
ENDIF
IF ( ASSOCIATED( grid%gd_cloud2 ) ) THEN 
  DEALLOCATE(grid%gd_cloud2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9845,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud2. ')
 endif
  NULLIFY(grid%gd_cloud2)
ENDIF
IF ( ASSOCIATED( grid%gd_cldfr ) ) THEN 
  DEALLOCATE(grid%gd_cldfr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9853,&
'frame/module_domain.f: Failed to deallocate grid%gd_cldfr. ')
 endif
  NULLIFY(grid%gd_cldfr)
ENDIF
IF ( ASSOCIATED( grid%raincv_a ) ) THEN 
  DEALLOCATE(grid%raincv_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9861,&
'frame/module_domain.f: Failed to deallocate grid%raincv_a. ')
 endif
  NULLIFY(grid%raincv_a)
ENDIF
IF ( ASSOCIATED( grid%raincv_b ) ) THEN 
  DEALLOCATE(grid%raincv_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9869,&
'frame/module_domain.f: Failed to deallocate grid%raincv_b. ')
 endif
  NULLIFY(grid%raincv_b)
ENDIF
IF ( ASSOCIATED( grid%gd_cloud_a ) ) THEN 
  DEALLOCATE(grid%gd_cloud_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9877,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud_a. ')
 endif
  NULLIFY(grid%gd_cloud_a)
ENDIF
IF ( ASSOCIATED( grid%gd_cloud2_a ) ) THEN 
  DEALLOCATE(grid%gd_cloud2_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9885,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud2_a. ')
 endif
  NULLIFY(grid%gd_cloud2_a)
ENDIF
IF ( ASSOCIATED( grid%qc_cu ) ) THEN 
  DEALLOCATE(grid%qc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9893,&
'frame/module_domain.f: Failed to deallocate grid%qc_cu. ')
 endif
  NULLIFY(grid%qc_cu)
ENDIF
IF ( ASSOCIATED( grid%qi_cu ) ) THEN 
  DEALLOCATE(grid%qi_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9901,&
'frame/module_domain.f: Failed to deallocate grid%qi_cu. ')
 endif
  NULLIFY(grid%qi_cu)
ENDIF
IF ( ASSOCIATED( grid%qr_cu ) ) THEN 
  DEALLOCATE(grid%qr_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9909,&
'frame/module_domain.f: Failed to deallocate grid%qr_cu. ')
 endif
  NULLIFY(grid%qr_cu)
ENDIF
IF ( ASSOCIATED( grid%qs_cu ) ) THEN 
  DEALLOCATE(grid%qs_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9917,&
'frame/module_domain.f: Failed to deallocate grid%qs_cu. ')
 endif
  NULLIFY(grid%qs_cu)
ENDIF
IF ( ASSOCIATED( grid%nc_cu ) ) THEN 
  DEALLOCATE(grid%nc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9925,&
'frame/module_domain.f: Failed to deallocate grid%nc_cu. ')
 endif
  NULLIFY(grid%nc_cu)
ENDIF
IF ( ASSOCIATED( grid%ni_cu ) ) THEN 
  DEALLOCATE(grid%ni_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9933,&
'frame/module_domain.f: Failed to deallocate grid%ni_cu. ')
 endif
  NULLIFY(grid%ni_cu)
ENDIF
IF ( ASSOCIATED( grid%nr_cu ) ) THEN 
  DEALLOCATE(grid%nr_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9941,&
'frame/module_domain.f: Failed to deallocate grid%nr_cu. ')
 endif
  NULLIFY(grid%nr_cu)
ENDIF
IF ( ASSOCIATED( grid%ns_cu ) ) THEN 
  DEALLOCATE(grid%ns_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9949,&
'frame/module_domain.f: Failed to deallocate grid%ns_cu. ')
 endif
  NULLIFY(grid%ns_cu)
ENDIF
IF ( ASSOCIATED( grid%ccn_cu ) ) THEN 
  DEALLOCATE(grid%ccn_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9957,&
'frame/module_domain.f: Failed to deallocate grid%ccn_cu. ')
 endif
  NULLIFY(grid%ccn_cu)
ENDIF
IF ( ASSOCIATED( grid%cu_uaf ) ) THEN 
  DEALLOCATE(grid%cu_uaf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9965,&
'frame/module_domain.f: Failed to deallocate grid%cu_uaf. ')
 endif
  NULLIFY(grid%cu_uaf)
ENDIF
IF ( ASSOCIATED( grid%efcs ) ) THEN 
  DEALLOCATE(grid%efcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9973,&
'frame/module_domain.f: Failed to deallocate grid%efcs. ')
 endif
  NULLIFY(grid%efcs)
ENDIF
IF ( ASSOCIATED( grid%efis ) ) THEN 
  DEALLOCATE(grid%efis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9981,&
'frame/module_domain.f: Failed to deallocate grid%efis. ')
 endif
  NULLIFY(grid%efis)
ENDIF
IF ( ASSOCIATED( grid%efcg ) ) THEN 
  DEALLOCATE(grid%efcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9989,&
'frame/module_domain.f: Failed to deallocate grid%efcg. ')
 endif
  NULLIFY(grid%efcg)
ENDIF
IF ( ASSOCIATED( grid%efig ) ) THEN 
  DEALLOCATE(grid%efig,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9997,&
'frame/module_domain.f: Failed to deallocate grid%efig. ')
 endif
  NULLIFY(grid%efig)
ENDIF
IF ( ASSOCIATED( grid%efsg ) ) THEN 
  DEALLOCATE(grid%efsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10005,&
'frame/module_domain.f: Failed to deallocate grid%efsg. ')
 endif
  NULLIFY(grid%efsg)
ENDIF
IF ( ASSOCIATED( grid%efss ) ) THEN 
  DEALLOCATE(grid%efss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10013,&
'frame/module_domain.f: Failed to deallocate grid%efss. ')
 endif
  NULLIFY(grid%efss)
ENDIF
IF ( ASSOCIATED( grid%wact ) ) THEN 
  DEALLOCATE(grid%wact,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10021,&
'frame/module_domain.f: Failed to deallocate grid%wact. ')
 endif
  NULLIFY(grid%wact)
ENDIF
IF ( ASSOCIATED( grid%ccn1_gs ) ) THEN 
  DEALLOCATE(grid%ccn1_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10029,&
'frame/module_domain.f: Failed to deallocate grid%ccn1_gs. ')
 endif
  NULLIFY(grid%ccn1_gs)
ENDIF
IF ( ASSOCIATED( grid%ccn2_gs ) ) THEN 
  DEALLOCATE(grid%ccn2_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10037,&
'frame/module_domain.f: Failed to deallocate grid%ccn2_gs. ')
 endif
  NULLIFY(grid%ccn2_gs)
ENDIF
IF ( ASSOCIATED( grid%ccn3_gs ) ) THEN 
  DEALLOCATE(grid%ccn3_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10045,&
'frame/module_domain.f: Failed to deallocate grid%ccn3_gs. ')
 endif
  NULLIFY(grid%ccn3_gs)
ENDIF
IF ( ASSOCIATED( grid%ccn4_gs ) ) THEN 
  DEALLOCATE(grid%ccn4_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10053,&
'frame/module_domain.f: Failed to deallocate grid%ccn4_gs. ')
 endif
  NULLIFY(grid%ccn4_gs)
ENDIF
IF ( ASSOCIATED( grid%ccn5_gs ) ) THEN 
  DEALLOCATE(grid%ccn5_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10061,&
'frame/module_domain.f: Failed to deallocate grid%ccn5_gs. ')
 endif
  NULLIFY(grid%ccn5_gs)
ENDIF
IF ( ASSOCIATED( grid%ccn6_gs ) ) THEN 
  DEALLOCATE(grid%ccn6_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10069,&
'frame/module_domain.f: Failed to deallocate grid%ccn6_gs. ')
 endif
  NULLIFY(grid%ccn6_gs)
ENDIF
IF ( ASSOCIATED( grid%ccn7_gs ) ) THEN 
  DEALLOCATE(grid%ccn7_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10077,&
'frame/module_domain.f: Failed to deallocate grid%ccn7_gs. ')
 endif
  NULLIFY(grid%ccn7_gs)
ENDIF
IF ( ASSOCIATED( grid%qc_bl ) ) THEN 
  DEALLOCATE(grid%qc_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10085,&
'frame/module_domain.f: Failed to deallocate grid%qc_bl. ')
 endif
  NULLIFY(grid%qc_bl)
ENDIF
IF ( ASSOCIATED( grid%qi_bl ) ) THEN 
  DEALLOCATE(grid%qi_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10093,&
'frame/module_domain.f: Failed to deallocate grid%qi_bl. ')
 endif
  NULLIFY(grid%qi_bl)
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10101,&
'frame/module_domain.f: Failed to deallocate grid%rthften. ')
 endif
  NULLIFY(grid%rthften)
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10109,&
'frame/module_domain.f: Failed to deallocate grid%rqvften. ')
 endif
  NULLIFY(grid%rqvften)
ENDIF
IF ( ASSOCIATED( grid%rthraten ) ) THEN 
  DEALLOCATE(grid%rthraten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10117,&
'frame/module_domain.f: Failed to deallocate grid%rthraten. ')
 endif
  NULLIFY(grid%rthraten)
ENDIF
IF ( ASSOCIATED( grid%rthratenlw ) ) THEN 
  DEALLOCATE(grid%rthratenlw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10125,&
'frame/module_domain.f: Failed to deallocate grid%rthratenlw. ')
 endif
  NULLIFY(grid%rthratenlw)
ENDIF
IF ( ASSOCIATED( grid%rthratenlwc ) ) THEN 
  DEALLOCATE(grid%rthratenlwc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10133,&
'frame/module_domain.f: Failed to deallocate grid%rthratenlwc. ')
 endif
  NULLIFY(grid%rthratenlwc)
ENDIF
IF ( ASSOCIATED( grid%rthratensw ) ) THEN 
  DEALLOCATE(grid%rthratensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10141,&
'frame/module_domain.f: Failed to deallocate grid%rthratensw. ')
 endif
  NULLIFY(grid%rthratensw)
ENDIF
IF ( ASSOCIATED( grid%rthratenswc ) ) THEN 
  DEALLOCATE(grid%rthratenswc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10149,&
'frame/module_domain.f: Failed to deallocate grid%rthratenswc. ')
 endif
  NULLIFY(grid%rthratenswc)
ENDIF
IF ( ASSOCIATED( grid%cldfra ) ) THEN 
  DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10157,&
'frame/module_domain.f: Failed to deallocate grid%cldfra. ')
 endif
  NULLIFY(grid%cldfra)
ENDIF
IF ( ASSOCIATED( grid%convcld ) ) THEN 
  DEALLOCATE(grid%convcld,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10165,&
'frame/module_domain.f: Failed to deallocate grid%convcld. ')
 endif
  NULLIFY(grid%convcld)
ENDIF
IF ( ASSOCIATED( grid%ccldfra ) ) THEN 
  DEALLOCATE(grid%ccldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10173,&
'frame/module_domain.f: Failed to deallocate grid%ccldfra. ')
 endif
  NULLIFY(grid%ccldfra)
ENDIF
IF ( ASSOCIATED( grid%cldfra_old ) ) THEN 
  DEALLOCATE(grid%cldfra_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10181,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_old. ')
 endif
  NULLIFY(grid%cldfra_old)
ENDIF
IF ( ASSOCIATED( grid%cldfra_bl ) ) THEN 
  DEALLOCATE(grid%cldfra_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10189,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_bl. ')
 endif
  NULLIFY(grid%cldfra_bl)
ENDIF
IF ( ASSOCIATED( grid%cldt ) ) THEN 
  DEALLOCATE(grid%cldt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10197,&
'frame/module_domain.f: Failed to deallocate grid%cldt. ')
 endif
  NULLIFY(grid%cldt)
ENDIF
IF ( ASSOCIATED( grid%swdown ) ) THEN 
  DEALLOCATE(grid%swdown,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10205,&
'frame/module_domain.f: Failed to deallocate grid%swdown. ')
 endif
  NULLIFY(grid%swdown)
ENDIF
IF ( ASSOCIATED( grid%swdown2 ) ) THEN 
  DEALLOCATE(grid%swdown2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10213,&
'frame/module_domain.f: Failed to deallocate grid%swdown2. ')
 endif
  NULLIFY(grid%swdown2)
ENDIF
IF ( ASSOCIATED( grid%swdownc ) ) THEN 
  DEALLOCATE(grid%swdownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10221,&
'frame/module_domain.f: Failed to deallocate grid%swdownc. ')
 endif
  NULLIFY(grid%swdownc)
ENDIF
IF ( ASSOCIATED( grid%swdownc2 ) ) THEN 
  DEALLOCATE(grid%swdownc2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10229,&
'frame/module_domain.f: Failed to deallocate grid%swdownc2. ')
 endif
  NULLIFY(grid%swdownc2)
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10237,&
'frame/module_domain.f: Failed to deallocate grid%gsw. ')
 endif
  NULLIFY(grid%gsw)
ENDIF
IF ( ASSOCIATED( grid%glw ) ) THEN 
  DEALLOCATE(grid%glw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10245,&
'frame/module_domain.f: Failed to deallocate grid%glw. ')
 endif
  NULLIFY(grid%glw)
ENDIF
IF ( ASSOCIATED( grid%swnorm ) ) THEN 
  DEALLOCATE(grid%swnorm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10253,&
'frame/module_domain.f: Failed to deallocate grid%swnorm. ')
 endif
  NULLIFY(grid%swnorm)
ENDIF
IF ( ASSOCIATED( grid%diffuse_frac ) ) THEN 
  DEALLOCATE(grid%diffuse_frac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10261,&
'frame/module_domain.f: Failed to deallocate grid%diffuse_frac. ')
 endif
  NULLIFY(grid%diffuse_frac)
ENDIF
IF ( ASSOCIATED( grid%swddir ) ) THEN 
  DEALLOCATE(grid%swddir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10269,&
'frame/module_domain.f: Failed to deallocate grid%swddir. ')
 endif
  NULLIFY(grid%swddir)
ENDIF
IF ( ASSOCIATED( grid%swddir2 ) ) THEN 
  DEALLOCATE(grid%swddir2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10277,&
'frame/module_domain.f: Failed to deallocate grid%swddir2. ')
 endif
  NULLIFY(grid%swddir2)
ENDIF
IF ( ASSOCIATED( grid%swddirc ) ) THEN 
  DEALLOCATE(grid%swddirc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10285,&
'frame/module_domain.f: Failed to deallocate grid%swddirc. ')
 endif
  NULLIFY(grid%swddirc)
ENDIF
IF ( ASSOCIATED( grid%swddni ) ) THEN 
  DEALLOCATE(grid%swddni,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10293,&
'frame/module_domain.f: Failed to deallocate grid%swddni. ')
 endif
  NULLIFY(grid%swddni)
ENDIF
IF ( ASSOCIATED( grid%swddni2 ) ) THEN 
  DEALLOCATE(grid%swddni2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10301,&
'frame/module_domain.f: Failed to deallocate grid%swddni2. ')
 endif
  NULLIFY(grid%swddni2)
ENDIF
IF ( ASSOCIATED( grid%swddnic ) ) THEN 
  DEALLOCATE(grid%swddnic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10309,&
'frame/module_domain.f: Failed to deallocate grid%swddnic. ')
 endif
  NULLIFY(grid%swddnic)
ENDIF
IF ( ASSOCIATED( grid%swddnic2 ) ) THEN 
  DEALLOCATE(grid%swddnic2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10317,&
'frame/module_domain.f: Failed to deallocate grid%swddnic2. ')
 endif
  NULLIFY(grid%swddnic2)
ENDIF
IF ( ASSOCIATED( grid%swddif ) ) THEN 
  DEALLOCATE(grid%swddif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10325,&
'frame/module_domain.f: Failed to deallocate grid%swddif. ')
 endif
  NULLIFY(grid%swddif)
ENDIF
IF ( ASSOCIATED( grid%swddif2 ) ) THEN 
  DEALLOCATE(grid%swddif2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10333,&
'frame/module_domain.f: Failed to deallocate grid%swddif2. ')
 endif
  NULLIFY(grid%swddif2)
ENDIF
IF ( ASSOCIATED( grid%gx ) ) THEN 
  DEALLOCATE(grid%gx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10341,&
'frame/module_domain.f: Failed to deallocate grid%gx. ')
 endif
  NULLIFY(grid%gx)
ENDIF
IF ( ASSOCIATED( grid%bx ) ) THEN 
  DEALLOCATE(grid%bx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10349,&
'frame/module_domain.f: Failed to deallocate grid%bx. ')
 endif
  NULLIFY(grid%bx)
ENDIF
IF ( ASSOCIATED( grid%gg ) ) THEN 
  DEALLOCATE(grid%gg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10357,&
'frame/module_domain.f: Failed to deallocate grid%gg. ')
 endif
  NULLIFY(grid%gg)
ENDIF
IF ( ASSOCIATED( grid%bb ) ) THEN 
  DEALLOCATE(grid%bb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10365,&
'frame/module_domain.f: Failed to deallocate grid%bb. ')
 endif
  NULLIFY(grid%bb)
ENDIF
IF ( ASSOCIATED( grid%coszen_ref ) ) THEN 
  DEALLOCATE(grid%coszen_ref,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10373,&
'frame/module_domain.f: Failed to deallocate grid%coszen_ref. ')
 endif
  NULLIFY(grid%coszen_ref)
ENDIF
IF ( ASSOCIATED( grid%swdown_ref ) ) THEN 
  DEALLOCATE(grid%swdown_ref,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10381,&
'frame/module_domain.f: Failed to deallocate grid%swdown_ref. ')
 endif
  NULLIFY(grid%swdown_ref)
ENDIF
IF ( ASSOCIATED( grid%swddir_ref ) ) THEN 
  DEALLOCATE(grid%swddir_ref,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10389,&
'frame/module_domain.f: Failed to deallocate grid%swddir_ref. ')
 endif
  NULLIFY(grid%swddir_ref)
ENDIF
IF ( ASSOCIATED( grid%aod5502d ) ) THEN 
  DEALLOCATE(grid%aod5502d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10397,&
'frame/module_domain.f: Failed to deallocate grid%aod5502d. ')
 endif
  NULLIFY(grid%aod5502d)
ENDIF
IF ( ASSOCIATED( grid%angexp2d ) ) THEN 
  DEALLOCATE(grid%angexp2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10405,&
'frame/module_domain.f: Failed to deallocate grid%angexp2d. ')
 endif
  NULLIFY(grid%angexp2d)
ENDIF
IF ( ASSOCIATED( grid%aerssa2d ) ) THEN 
  DEALLOCATE(grid%aerssa2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10413,&
'frame/module_domain.f: Failed to deallocate grid%aerssa2d. ')
 endif
  NULLIFY(grid%aerssa2d)
ENDIF
IF ( ASSOCIATED( grid%aerasy2d ) ) THEN 
  DEALLOCATE(grid%aerasy2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10421,&
'frame/module_domain.f: Failed to deallocate grid%aerasy2d. ')
 endif
  NULLIFY(grid%aerasy2d)
ENDIF
IF ( ASSOCIATED( grid%aod5503d ) ) THEN 
  DEALLOCATE(grid%aod5503d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10429,&
'frame/module_domain.f: Failed to deallocate grid%aod5503d. ')
 endif
  NULLIFY(grid%aod5503d)
ENDIF
IF ( ASSOCIATED( grid%taod5503d ) ) THEN 
  DEALLOCATE(grid%taod5503d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10437,&
'frame/module_domain.f: Failed to deallocate grid%taod5503d. ')
 endif
  NULLIFY(grid%taod5503d)
ENDIF
IF ( ASSOCIATED( grid%taod5502d ) ) THEN 
  DEALLOCATE(grid%taod5502d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10445,&
'frame/module_domain.f: Failed to deallocate grid%taod5502d. ')
 endif
  NULLIFY(grid%taod5502d)
ENDIF
IF ( ASSOCIATED( grid%t2min ) ) THEN 
  DEALLOCATE(grid%t2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10453,&
'frame/module_domain.f: Failed to deallocate grid%t2min. ')
 endif
  NULLIFY(grid%t2min)
ENDIF
IF ( ASSOCIATED( grid%t2max ) ) THEN 
  DEALLOCATE(grid%t2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10461,&
'frame/module_domain.f: Failed to deallocate grid%t2max. ')
 endif
  NULLIFY(grid%t2max)
ENDIF
IF ( ASSOCIATED( grid%tt2min ) ) THEN 
  DEALLOCATE(grid%tt2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10469,&
'frame/module_domain.f: Failed to deallocate grid%tt2min. ')
 endif
  NULLIFY(grid%tt2min)
ENDIF
IF ( ASSOCIATED( grid%tt2max ) ) THEN 
  DEALLOCATE(grid%tt2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10477,&
'frame/module_domain.f: Failed to deallocate grid%tt2max. ')
 endif
  NULLIFY(grid%tt2max)
ENDIF
IF ( ASSOCIATED( grid%t2mean ) ) THEN 
  DEALLOCATE(grid%t2mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10485,&
'frame/module_domain.f: Failed to deallocate grid%t2mean. ')
 endif
  NULLIFY(grid%t2mean)
ENDIF
IF ( ASSOCIATED( grid%t2std ) ) THEN 
  DEALLOCATE(grid%t2std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10493,&
'frame/module_domain.f: Failed to deallocate grid%t2std. ')
 endif
  NULLIFY(grid%t2std)
ENDIF
IF ( ASSOCIATED( grid%q2min ) ) THEN 
  DEALLOCATE(grid%q2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10501,&
'frame/module_domain.f: Failed to deallocate grid%q2min. ')
 endif
  NULLIFY(grid%q2min)
ENDIF
IF ( ASSOCIATED( grid%q2max ) ) THEN 
  DEALLOCATE(grid%q2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10509,&
'frame/module_domain.f: Failed to deallocate grid%q2max. ')
 endif
  NULLIFY(grid%q2max)
ENDIF
IF ( ASSOCIATED( grid%tq2min ) ) THEN 
  DEALLOCATE(grid%tq2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10517,&
'frame/module_domain.f: Failed to deallocate grid%tq2min. ')
 endif
  NULLIFY(grid%tq2min)
ENDIF
IF ( ASSOCIATED( grid%tq2max ) ) THEN 
  DEALLOCATE(grid%tq2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10525,&
'frame/module_domain.f: Failed to deallocate grid%tq2max. ')
 endif
  NULLIFY(grid%tq2max)
ENDIF
IF ( ASSOCIATED( grid%q2mean ) ) THEN 
  DEALLOCATE(grid%q2mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10533,&
'frame/module_domain.f: Failed to deallocate grid%q2mean. ')
 endif
  NULLIFY(grid%q2mean)
ENDIF
IF ( ASSOCIATED( grid%q2std ) ) THEN 
  DEALLOCATE(grid%q2std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10541,&
'frame/module_domain.f: Failed to deallocate grid%q2std. ')
 endif
  NULLIFY(grid%q2std)
ENDIF
IF ( ASSOCIATED( grid%skintempmin ) ) THEN 
  DEALLOCATE(grid%skintempmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10549,&
'frame/module_domain.f: Failed to deallocate grid%skintempmin. ')
 endif
  NULLIFY(grid%skintempmin)
ENDIF
IF ( ASSOCIATED( grid%skintempmax ) ) THEN 
  DEALLOCATE(grid%skintempmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10557,&
'frame/module_domain.f: Failed to deallocate grid%skintempmax. ')
 endif
  NULLIFY(grid%skintempmax)
ENDIF
IF ( ASSOCIATED( grid%tskintempmin ) ) THEN 
  DEALLOCATE(grid%tskintempmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10565,&
'frame/module_domain.f: Failed to deallocate grid%tskintempmin. ')
 endif
  NULLIFY(grid%tskintempmin)
ENDIF
IF ( ASSOCIATED( grid%tskintempmax ) ) THEN 
  DEALLOCATE(grid%tskintempmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10573,&
'frame/module_domain.f: Failed to deallocate grid%tskintempmax. ')
 endif
  NULLIFY(grid%tskintempmax)
ENDIF
IF ( ASSOCIATED( grid%skintempmean ) ) THEN 
  DEALLOCATE(grid%skintempmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10581,&
'frame/module_domain.f: Failed to deallocate grid%skintempmean. ')
 endif
  NULLIFY(grid%skintempmean)
ENDIF
IF ( ASSOCIATED( grid%skintempstd ) ) THEN 
  DEALLOCATE(grid%skintempstd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10589,&
'frame/module_domain.f: Failed to deallocate grid%skintempstd. ')
 endif
  NULLIFY(grid%skintempstd)
ENDIF
IF ( ASSOCIATED( grid%u10max ) ) THEN 
  DEALLOCATE(grid%u10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10597,&
'frame/module_domain.f: Failed to deallocate grid%u10max. ')
 endif
  NULLIFY(grid%u10max)
ENDIF
IF ( ASSOCIATED( grid%v10max ) ) THEN 
  DEALLOCATE(grid%v10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10605,&
'frame/module_domain.f: Failed to deallocate grid%v10max. ')
 endif
  NULLIFY(grid%v10max)
ENDIF
IF ( ASSOCIATED( grid%spduv10max ) ) THEN 
  DEALLOCATE(grid%spduv10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10613,&
'frame/module_domain.f: Failed to deallocate grid%spduv10max. ')
 endif
  NULLIFY(grid%spduv10max)
ENDIF
IF ( ASSOCIATED( grid%tspduv10max ) ) THEN 
  DEALLOCATE(grid%tspduv10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10621,&
'frame/module_domain.f: Failed to deallocate grid%tspduv10max. ')
 endif
  NULLIFY(grid%tspduv10max)
ENDIF
IF ( ASSOCIATED( grid%u10mean ) ) THEN 
  DEALLOCATE(grid%u10mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10629,&
'frame/module_domain.f: Failed to deallocate grid%u10mean. ')
 endif
  NULLIFY(grid%u10mean)
ENDIF
IF ( ASSOCIATED( grid%v10mean ) ) THEN 
  DEALLOCATE(grid%v10mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10637,&
'frame/module_domain.f: Failed to deallocate grid%v10mean. ')
 endif
  NULLIFY(grid%v10mean)
ENDIF
IF ( ASSOCIATED( grid%spduv10mean ) ) THEN 
  DEALLOCATE(grid%spduv10mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10645,&
'frame/module_domain.f: Failed to deallocate grid%spduv10mean. ')
 endif
  NULLIFY(grid%spduv10mean)
ENDIF
IF ( ASSOCIATED( grid%u10std ) ) THEN 
  DEALLOCATE(grid%u10std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10653,&
'frame/module_domain.f: Failed to deallocate grid%u10std. ')
 endif
  NULLIFY(grid%u10std)
ENDIF
IF ( ASSOCIATED( grid%v10std ) ) THEN 
  DEALLOCATE(grid%v10std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10661,&
'frame/module_domain.f: Failed to deallocate grid%v10std. ')
 endif
  NULLIFY(grid%v10std)
ENDIF
IF ( ASSOCIATED( grid%spduv10std ) ) THEN 
  DEALLOCATE(grid%spduv10std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10669,&
'frame/module_domain.f: Failed to deallocate grid%spduv10std. ')
 endif
  NULLIFY(grid%spduv10std)
ENDIF
IF ( ASSOCIATED( grid%raincvmax ) ) THEN 
  DEALLOCATE(grid%raincvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10677,&
'frame/module_domain.f: Failed to deallocate grid%raincvmax. ')
 endif
  NULLIFY(grid%raincvmax)
ENDIF
IF ( ASSOCIATED( grid%rainncvmax ) ) THEN 
  DEALLOCATE(grid%rainncvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10685,&
'frame/module_domain.f: Failed to deallocate grid%rainncvmax. ')
 endif
  NULLIFY(grid%rainncvmax)
ENDIF
IF ( ASSOCIATED( grid%traincvmax ) ) THEN 
  DEALLOCATE(grid%traincvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10693,&
'frame/module_domain.f: Failed to deallocate grid%traincvmax. ')
 endif
  NULLIFY(grid%traincvmax)
ENDIF
IF ( ASSOCIATED( grid%trainncvmax ) ) THEN 
  DEALLOCATE(grid%trainncvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10701,&
'frame/module_domain.f: Failed to deallocate grid%trainncvmax. ')
 endif
  NULLIFY(grid%trainncvmax)
ENDIF
IF ( ASSOCIATED( grid%raincvmean ) ) THEN 
  DEALLOCATE(grid%raincvmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10709,&
'frame/module_domain.f: Failed to deallocate grid%raincvmean. ')
 endif
  NULLIFY(grid%raincvmean)
ENDIF
IF ( ASSOCIATED( grid%rainncvmean ) ) THEN 
  DEALLOCATE(grid%rainncvmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10717,&
'frame/module_domain.f: Failed to deallocate grid%rainncvmean. ')
 endif
  NULLIFY(grid%rainncvmean)
ENDIF
IF ( ASSOCIATED( grid%raincvstd ) ) THEN 
  DEALLOCATE(grid%raincvstd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10725,&
'frame/module_domain.f: Failed to deallocate grid%raincvstd. ')
 endif
  NULLIFY(grid%raincvstd)
ENDIF
IF ( ASSOCIATED( grid%rainncvstd ) ) THEN 
  DEALLOCATE(grid%rainncvstd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10733,&
'frame/module_domain.f: Failed to deallocate grid%rainncvstd. ')
 endif
  NULLIFY(grid%rainncvstd)
ENDIF
IF ( ASSOCIATED( grid%acswupt ) ) THEN 
  DEALLOCATE(grid%acswupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10741,&
'frame/module_domain.f: Failed to deallocate grid%acswupt. ')
 endif
  NULLIFY(grid%acswupt)
ENDIF
IF ( ASSOCIATED( grid%acswuptc ) ) THEN 
  DEALLOCATE(grid%acswuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10749,&
'frame/module_domain.f: Failed to deallocate grid%acswuptc. ')
 endif
  NULLIFY(grid%acswuptc)
ENDIF
IF ( ASSOCIATED( grid%acswdnt ) ) THEN 
  DEALLOCATE(grid%acswdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10757,&
'frame/module_domain.f: Failed to deallocate grid%acswdnt. ')
 endif
  NULLIFY(grid%acswdnt)
ENDIF
IF ( ASSOCIATED( grid%acswdntc ) ) THEN 
  DEALLOCATE(grid%acswdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10765,&
'frame/module_domain.f: Failed to deallocate grid%acswdntc. ')
 endif
  NULLIFY(grid%acswdntc)
ENDIF
IF ( ASSOCIATED( grid%acswupb ) ) THEN 
  DEALLOCATE(grid%acswupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10773,&
'frame/module_domain.f: Failed to deallocate grid%acswupb. ')
 endif
  NULLIFY(grid%acswupb)
ENDIF
IF ( ASSOCIATED( grid%acswupbc ) ) THEN 
  DEALLOCATE(grid%acswupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10781,&
'frame/module_domain.f: Failed to deallocate grid%acswupbc. ')
 endif
  NULLIFY(grid%acswupbc)
ENDIF
IF ( ASSOCIATED( grid%acswdnb ) ) THEN 
  DEALLOCATE(grid%acswdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10789,&
'frame/module_domain.f: Failed to deallocate grid%acswdnb. ')
 endif
  NULLIFY(grid%acswdnb)
ENDIF
IF ( ASSOCIATED( grid%acswdnbc ) ) THEN 
  DEALLOCATE(grid%acswdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10797,&
'frame/module_domain.f: Failed to deallocate grid%acswdnbc. ')
 endif
  NULLIFY(grid%acswdnbc)
ENDIF
IF ( ASSOCIATED( grid%aclwupt ) ) THEN 
  DEALLOCATE(grid%aclwupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10805,&
'frame/module_domain.f: Failed to deallocate grid%aclwupt. ')
 endif
  NULLIFY(grid%aclwupt)
ENDIF
IF ( ASSOCIATED( grid%aclwuptc ) ) THEN 
  DEALLOCATE(grid%aclwuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10813,&
'frame/module_domain.f: Failed to deallocate grid%aclwuptc. ')
 endif
  NULLIFY(grid%aclwuptc)
ENDIF
IF ( ASSOCIATED( grid%aclwdnt ) ) THEN 
  DEALLOCATE(grid%aclwdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10821,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnt. ')
 endif
  NULLIFY(grid%aclwdnt)
ENDIF
IF ( ASSOCIATED( grid%aclwdntc ) ) THEN 
  DEALLOCATE(grid%aclwdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10829,&
'frame/module_domain.f: Failed to deallocate grid%aclwdntc. ')
 endif
  NULLIFY(grid%aclwdntc)
ENDIF
IF ( ASSOCIATED( grid%aclwupb ) ) THEN 
  DEALLOCATE(grid%aclwupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10837,&
'frame/module_domain.f: Failed to deallocate grid%aclwupb. ')
 endif
  NULLIFY(grid%aclwupb)
ENDIF
IF ( ASSOCIATED( grid%aclwupbc ) ) THEN 
  DEALLOCATE(grid%aclwupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10845,&
'frame/module_domain.f: Failed to deallocate grid%aclwupbc. ')
 endif
  NULLIFY(grid%aclwupbc)
ENDIF
IF ( ASSOCIATED( grid%aclwdnb ) ) THEN 
  DEALLOCATE(grid%aclwdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10853,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnb. ')
 endif
  NULLIFY(grid%aclwdnb)
ENDIF
IF ( ASSOCIATED( grid%aclwdnbc ) ) THEN 
  DEALLOCATE(grid%aclwdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10861,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnbc. ')
 endif
  NULLIFY(grid%aclwdnbc)
ENDIF
IF ( ASSOCIATED( grid%i_acswupt ) ) THEN 
  DEALLOCATE(grid%i_acswupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10869,&
'frame/module_domain.f: Failed to deallocate grid%i_acswupt. ')
 endif
  NULLIFY(grid%i_acswupt)
ENDIF
IF ( ASSOCIATED( grid%i_acswuptc ) ) THEN 
  DEALLOCATE(grid%i_acswuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10877,&
'frame/module_domain.f: Failed to deallocate grid%i_acswuptc. ')
 endif
  NULLIFY(grid%i_acswuptc)
ENDIF
IF ( ASSOCIATED( grid%i_acswdnt ) ) THEN 
  DEALLOCATE(grid%i_acswdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10885,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdnt. ')
 endif
  NULLIFY(grid%i_acswdnt)
ENDIF
IF ( ASSOCIATED( grid%i_acswdntc ) ) THEN 
  DEALLOCATE(grid%i_acswdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10893,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdntc. ')
 endif
  NULLIFY(grid%i_acswdntc)
ENDIF
IF ( ASSOCIATED( grid%i_acswupb ) ) THEN 
  DEALLOCATE(grid%i_acswupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10901,&
'frame/module_domain.f: Failed to deallocate grid%i_acswupb. ')
 endif
  NULLIFY(grid%i_acswupb)
ENDIF
IF ( ASSOCIATED( grid%i_acswupbc ) ) THEN 
  DEALLOCATE(grid%i_acswupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10909,&
'frame/module_domain.f: Failed to deallocate grid%i_acswupbc. ')
 endif
  NULLIFY(grid%i_acswupbc)
ENDIF
IF ( ASSOCIATED( grid%i_acswdnb ) ) THEN 
  DEALLOCATE(grid%i_acswdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10917,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdnb. ')
 endif
  NULLIFY(grid%i_acswdnb)
ENDIF
IF ( ASSOCIATED( grid%i_acswdnbc ) ) THEN 
  DEALLOCATE(grid%i_acswdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10925,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdnbc. ')
 endif
  NULLIFY(grid%i_acswdnbc)
ENDIF
IF ( ASSOCIATED( grid%i_aclwupt ) ) THEN 
  DEALLOCATE(grid%i_aclwupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10933,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwupt. ')
 endif
  NULLIFY(grid%i_aclwupt)
ENDIF
IF ( ASSOCIATED( grid%i_aclwuptc ) ) THEN 
  DEALLOCATE(grid%i_aclwuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10941,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwuptc. ')
 endif
  NULLIFY(grid%i_aclwuptc)
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnt ) ) THEN 
  DEALLOCATE(grid%i_aclwdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10949,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnt. ')
 endif
  NULLIFY(grid%i_aclwdnt)
ENDIF
IF ( ASSOCIATED( grid%i_aclwdntc ) ) THEN 
  DEALLOCATE(grid%i_aclwdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10957,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdntc. ')
 endif
  NULLIFY(grid%i_aclwdntc)
ENDIF
IF ( ASSOCIATED( grid%i_aclwupb ) ) THEN 
  DEALLOCATE(grid%i_aclwupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10965,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwupb. ')
 endif
  NULLIFY(grid%i_aclwupb)
ENDIF
IF ( ASSOCIATED( grid%i_aclwupbc ) ) THEN 
  DEALLOCATE(grid%i_aclwupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10973,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwupbc. ')
 endif
  NULLIFY(grid%i_aclwupbc)
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnb ) ) THEN 
  DEALLOCATE(grid%i_aclwdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10981,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnb. ')
 endif
  NULLIFY(grid%i_aclwdnb)
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnbc ) ) THEN 
  DEALLOCATE(grid%i_aclwdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10989,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnbc. ')
 endif
  NULLIFY(grid%i_aclwdnbc)
ENDIF
IF ( ASSOCIATED( grid%swupt ) ) THEN 
  DEALLOCATE(grid%swupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10997,&
'frame/module_domain.f: Failed to deallocate grid%swupt. ')
 endif
  NULLIFY(grid%swupt)
ENDIF
IF ( ASSOCIATED( grid%swuptc ) ) THEN 
  DEALLOCATE(grid%swuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11005,&
'frame/module_domain.f: Failed to deallocate grid%swuptc. ')
 endif
  NULLIFY(grid%swuptc)
ENDIF
IF ( ASSOCIATED( grid%swuptcln ) ) THEN 
  DEALLOCATE(grid%swuptcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11013,&
'frame/module_domain.f: Failed to deallocate grid%swuptcln. ')
 endif
  NULLIFY(grid%swuptcln)
ENDIF
IF ( ASSOCIATED( grid%swdnt ) ) THEN 
  DEALLOCATE(grid%swdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11021,&
'frame/module_domain.f: Failed to deallocate grid%swdnt. ')
 endif
  NULLIFY(grid%swdnt)
ENDIF
IF ( ASSOCIATED( grid%swdntc ) ) THEN 
  DEALLOCATE(grid%swdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11029,&
'frame/module_domain.f: Failed to deallocate grid%swdntc. ')
 endif
  NULLIFY(grid%swdntc)
ENDIF
IF ( ASSOCIATED( grid%swdntcln ) ) THEN 
  DEALLOCATE(grid%swdntcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11037,&
'frame/module_domain.f: Failed to deallocate grid%swdntcln. ')
 endif
  NULLIFY(grid%swdntcln)
ENDIF
IF ( ASSOCIATED( grid%swupb ) ) THEN 
  DEALLOCATE(grid%swupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11045,&
'frame/module_domain.f: Failed to deallocate grid%swupb. ')
 endif
  NULLIFY(grid%swupb)
ENDIF
IF ( ASSOCIATED( grid%swupbc ) ) THEN 
  DEALLOCATE(grid%swupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11053,&
'frame/module_domain.f: Failed to deallocate grid%swupbc. ')
 endif
  NULLIFY(grid%swupbc)
ENDIF
IF ( ASSOCIATED( grid%swupbcln ) ) THEN 
  DEALLOCATE(grid%swupbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11061,&
'frame/module_domain.f: Failed to deallocate grid%swupbcln. ')
 endif
  NULLIFY(grid%swupbcln)
ENDIF
IF ( ASSOCIATED( grid%swdnb ) ) THEN 
  DEALLOCATE(grid%swdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11069,&
'frame/module_domain.f: Failed to deallocate grid%swdnb. ')
 endif
  NULLIFY(grid%swdnb)
ENDIF
IF ( ASSOCIATED( grid%swdnbc ) ) THEN 
  DEALLOCATE(grid%swdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11077,&
'frame/module_domain.f: Failed to deallocate grid%swdnbc. ')
 endif
  NULLIFY(grid%swdnbc)
ENDIF
IF ( ASSOCIATED( grid%swdnbcln ) ) THEN 
  DEALLOCATE(grid%swdnbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11085,&
'frame/module_domain.f: Failed to deallocate grid%swdnbcln. ')
 endif
  NULLIFY(grid%swdnbcln)
ENDIF
IF ( ASSOCIATED( grid%lwupt ) ) THEN 
  DEALLOCATE(grid%lwupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11093,&
'frame/module_domain.f: Failed to deallocate grid%lwupt. ')
 endif
  NULLIFY(grid%lwupt)
ENDIF
IF ( ASSOCIATED( grid%lwuptc ) ) THEN 
  DEALLOCATE(grid%lwuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11101,&
'frame/module_domain.f: Failed to deallocate grid%lwuptc. ')
 endif
  NULLIFY(grid%lwuptc)
ENDIF
IF ( ASSOCIATED( grid%lwuptcln ) ) THEN 
  DEALLOCATE(grid%lwuptcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11109,&
'frame/module_domain.f: Failed to deallocate grid%lwuptcln. ')
 endif
  NULLIFY(grid%lwuptcln)
ENDIF
IF ( ASSOCIATED( grid%lwdnt ) ) THEN 
  DEALLOCATE(grid%lwdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11117,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt. ')
 endif
  NULLIFY(grid%lwdnt)
ENDIF
IF ( ASSOCIATED( grid%lwdntc ) ) THEN 
  DEALLOCATE(grid%lwdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11125,&
'frame/module_domain.f: Failed to deallocate grid%lwdntc. ')
 endif
  NULLIFY(grid%lwdntc)
ENDIF
IF ( ASSOCIATED( grid%lwdntcln ) ) THEN 
  DEALLOCATE(grid%lwdntcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11133,&
'frame/module_domain.f: Failed to deallocate grid%lwdntcln. ')
 endif
  NULLIFY(grid%lwdntcln)
ENDIF
IF ( ASSOCIATED( grid%lwupb ) ) THEN 
  DEALLOCATE(grid%lwupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11141,&
'frame/module_domain.f: Failed to deallocate grid%lwupb. ')
 endif
  NULLIFY(grid%lwupb)
ENDIF
IF ( ASSOCIATED( grid%lwupbc ) ) THEN 
  DEALLOCATE(grid%lwupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11149,&
'frame/module_domain.f: Failed to deallocate grid%lwupbc. ')
 endif
  NULLIFY(grid%lwupbc)
ENDIF
IF ( ASSOCIATED( grid%lwupbcln ) ) THEN 
  DEALLOCATE(grid%lwupbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11157,&
'frame/module_domain.f: Failed to deallocate grid%lwupbcln. ')
 endif
  NULLIFY(grid%lwupbcln)
ENDIF
IF ( ASSOCIATED( grid%lwdnb ) ) THEN 
  DEALLOCATE(grid%lwdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11165,&
'frame/module_domain.f: Failed to deallocate grid%lwdnb. ')
 endif
  NULLIFY(grid%lwdnb)
ENDIF
IF ( ASSOCIATED( grid%lwdnbc ) ) THEN 
  DEALLOCATE(grid%lwdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11173,&
'frame/module_domain.f: Failed to deallocate grid%lwdnbc. ')
 endif
  NULLIFY(grid%lwdnbc)
ENDIF
IF ( ASSOCIATED( grid%lwdnbcln ) ) THEN 
  DEALLOCATE(grid%lwdnbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11181,&
'frame/module_domain.f: Failed to deallocate grid%lwdnbcln. ')
 endif
  NULLIFY(grid%lwdnbcln)
ENDIF
IF ( ASSOCIATED( grid%swcf ) ) THEN 
  DEALLOCATE(grid%swcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11189,&
'frame/module_domain.f: Failed to deallocate grid%swcf. ')
 endif
  NULLIFY(grid%swcf)
ENDIF
IF ( ASSOCIATED( grid%lwcf ) ) THEN 
  DEALLOCATE(grid%lwcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11197,&
'frame/module_domain.f: Failed to deallocate grid%lwcf. ')
 endif
  NULLIFY(grid%lwcf)
ENDIF
IF ( ASSOCIATED( grid%olr ) ) THEN 
  DEALLOCATE(grid%olr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11205,&
'frame/module_domain.f: Failed to deallocate grid%olr. ')
 endif
  NULLIFY(grid%olr)
ENDIF
IF ( ASSOCIATED( grid%xlat_u ) ) THEN 
  DEALLOCATE(grid%xlat_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11213,&
'frame/module_domain.f: Failed to deallocate grid%xlat_u. ')
 endif
  NULLIFY(grid%xlat_u)
ENDIF
IF ( ASSOCIATED( grid%xlong_u ) ) THEN 
  DEALLOCATE(grid%xlong_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11221,&
'frame/module_domain.f: Failed to deallocate grid%xlong_u. ')
 endif
  NULLIFY(grid%xlong_u)
ENDIF
IF ( ASSOCIATED( grid%xlat_v ) ) THEN 
  DEALLOCATE(grid%xlat_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11229,&
'frame/module_domain.f: Failed to deallocate grid%xlat_v. ')
 endif
  NULLIFY(grid%xlat_v)
ENDIF
IF ( ASSOCIATED( grid%xlong_v ) ) THEN 
  DEALLOCATE(grid%xlong_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11237,&
'frame/module_domain.f: Failed to deallocate grid%xlong_v. ')
 endif
  NULLIFY(grid%xlong_v)
ENDIF
IF ( ASSOCIATED( grid%albedo ) ) THEN 
  DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11245,&
'frame/module_domain.f: Failed to deallocate grid%albedo. ')
 endif
  NULLIFY(grid%albedo)
ENDIF
IF ( ASSOCIATED( grid%clat ) ) THEN 
  DEALLOCATE(grid%clat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11253,&
'frame/module_domain.f: Failed to deallocate grid%clat. ')
 endif
  NULLIFY(grid%clat)
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11261,&
'frame/module_domain.f: Failed to deallocate grid%albbck. ')
 endif
  NULLIFY(grid%albbck)
ENDIF
IF ( ASSOCIATED( grid%embck ) ) THEN 
  DEALLOCATE(grid%embck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11269,&
'frame/module_domain.f: Failed to deallocate grid%embck. ')
 endif
  NULLIFY(grid%embck)
ENDIF
IF ( ASSOCIATED( grid%emiss ) ) THEN 
  DEALLOCATE(grid%emiss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11277,&
'frame/module_domain.f: Failed to deallocate grid%emiss. ')
 endif
  NULLIFY(grid%emiss)
ENDIF
IF ( ASSOCIATED( grid%snotime ) ) THEN 
  DEALLOCATE(grid%snotime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11285,&
'frame/module_domain.f: Failed to deallocate grid%snotime. ')
 endif
  NULLIFY(grid%snotime)
ENDIF
IF ( ASSOCIATED( grid%noahres ) ) THEN 
  DEALLOCATE(grid%noahres,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11293,&
'frame/module_domain.f: Failed to deallocate grid%noahres. ')
 endif
  NULLIFY(grid%noahres)
ENDIF
IF ( ASSOCIATED( grid%cldefi ) ) THEN 
  DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11301,&
'frame/module_domain.f: Failed to deallocate grid%cldefi. ')
 endif
  NULLIFY(grid%cldefi)
ENDIF
IF ( ASSOCIATED( grid%rublten ) ) THEN 
  DEALLOCATE(grid%rublten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11309,&
'frame/module_domain.f: Failed to deallocate grid%rublten. ')
 endif
  NULLIFY(grid%rublten)
ENDIF
IF ( ASSOCIATED( grid%rvblten ) ) THEN 
  DEALLOCATE(grid%rvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11317,&
'frame/module_domain.f: Failed to deallocate grid%rvblten. ')
 endif
  NULLIFY(grid%rvblten)
ENDIF
IF ( ASSOCIATED( grid%rthblten ) ) THEN 
  DEALLOCATE(grid%rthblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11325,&
'frame/module_domain.f: Failed to deallocate grid%rthblten. ')
 endif
  NULLIFY(grid%rthblten)
ENDIF
IF ( ASSOCIATED( grid%rqvblten ) ) THEN 
  DEALLOCATE(grid%rqvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11333,&
'frame/module_domain.f: Failed to deallocate grid%rqvblten. ')
 endif
  NULLIFY(grid%rqvblten)
ENDIF
IF ( ASSOCIATED( grid%rqcblten ) ) THEN 
  DEALLOCATE(grid%rqcblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11341,&
'frame/module_domain.f: Failed to deallocate grid%rqcblten. ')
 endif
  NULLIFY(grid%rqcblten)
ENDIF
IF ( ASSOCIATED( grid%rqiblten ) ) THEN 
  DEALLOCATE(grid%rqiblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11349,&
'frame/module_domain.f: Failed to deallocate grid%rqiblten. ')
 endif
  NULLIFY(grid%rqiblten)
ENDIF
IF ( ASSOCIATED( grid%rqniblten ) ) THEN 
  DEALLOCATE(grid%rqniblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11357,&
'frame/module_domain.f: Failed to deallocate grid%rqniblten. ')
 endif
  NULLIFY(grid%rqniblten)
ENDIF
IF ( ASSOCIATED( grid%flx4 ) ) THEN 
  DEALLOCATE(grid%flx4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11365,&
'frame/module_domain.f: Failed to deallocate grid%flx4. ')
 endif
  NULLIFY(grid%flx4)
ENDIF
IF ( ASSOCIATED( grid%fvb ) ) THEN 
  DEALLOCATE(grid%fvb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11373,&
'frame/module_domain.f: Failed to deallocate grid%fvb. ')
 endif
  NULLIFY(grid%fvb)
ENDIF
IF ( ASSOCIATED( grid%fbur ) ) THEN 
  DEALLOCATE(grid%fbur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11381,&
'frame/module_domain.f: Failed to deallocate grid%fbur. ')
 endif
  NULLIFY(grid%fbur)
ENDIF
IF ( ASSOCIATED( grid%fgsn ) ) THEN 
  DEALLOCATE(grid%fgsn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11389,&
'frame/module_domain.f: Failed to deallocate grid%fgsn. ')
 endif
  NULLIFY(grid%fgsn)
ENDIF
IF ( ASSOCIATED( grid%tsk_mosaic ) ) THEN 
  DEALLOCATE(grid%tsk_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11397,&
'frame/module_domain.f: Failed to deallocate grid%tsk_mosaic. ')
 endif
  NULLIFY(grid%tsk_mosaic)
ENDIF
IF ( ASSOCIATED( grid%qsfc_mosaic ) ) THEN 
  DEALLOCATE(grid%qsfc_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11405,&
'frame/module_domain.f: Failed to deallocate grid%qsfc_mosaic. ')
 endif
  NULLIFY(grid%qsfc_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tslb_mosaic ) ) THEN 
  DEALLOCATE(grid%tslb_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11413,&
'frame/module_domain.f: Failed to deallocate grid%tslb_mosaic. ')
 endif
  NULLIFY(grid%tslb_mosaic)
ENDIF
IF ( ASSOCIATED( grid%smois_mosaic ) ) THEN 
  DEALLOCATE(grid%smois_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11421,&
'frame/module_domain.f: Failed to deallocate grid%smois_mosaic. ')
 endif
  NULLIFY(grid%smois_mosaic)
ENDIF
IF ( ASSOCIATED( grid%sh2o_mosaic ) ) THEN 
  DEALLOCATE(grid%sh2o_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11429,&
'frame/module_domain.f: Failed to deallocate grid%sh2o_mosaic. ')
 endif
  NULLIFY(grid%sh2o_mosaic)
ENDIF
IF ( ASSOCIATED( grid%canwat_mosaic ) ) THEN 
  DEALLOCATE(grid%canwat_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11437,&
'frame/module_domain.f: Failed to deallocate grid%canwat_mosaic. ')
 endif
  NULLIFY(grid%canwat_mosaic)
ENDIF
IF ( ASSOCIATED( grid%snow_mosaic ) ) THEN 
  DEALLOCATE(grid%snow_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11445,&
'frame/module_domain.f: Failed to deallocate grid%snow_mosaic. ')
 endif
  NULLIFY(grid%snow_mosaic)
ENDIF
IF ( ASSOCIATED( grid%snowh_mosaic ) ) THEN 
  DEALLOCATE(grid%snowh_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11453,&
'frame/module_domain.f: Failed to deallocate grid%snowh_mosaic. ')
 endif
  NULLIFY(grid%snowh_mosaic)
ENDIF
IF ( ASSOCIATED( grid%snowc_mosaic ) ) THEN 
  DEALLOCATE(grid%snowc_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11461,&
'frame/module_domain.f: Failed to deallocate grid%snowc_mosaic. ')
 endif
  NULLIFY(grid%snowc_mosaic)
ENDIF
IF ( ASSOCIATED( grid%albedo_mosaic ) ) THEN 
  DEALLOCATE(grid%albedo_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11469,&
'frame/module_domain.f: Failed to deallocate grid%albedo_mosaic. ')
 endif
  NULLIFY(grid%albedo_mosaic)
ENDIF
IF ( ASSOCIATED( grid%albbck_mosaic ) ) THEN 
  DEALLOCATE(grid%albbck_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11477,&
'frame/module_domain.f: Failed to deallocate grid%albbck_mosaic. ')
 endif
  NULLIFY(grid%albbck_mosaic)
ENDIF
IF ( ASSOCIATED( grid%emiss_mosaic ) ) THEN 
  DEALLOCATE(grid%emiss_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11485,&
'frame/module_domain.f: Failed to deallocate grid%emiss_mosaic. ')
 endif
  NULLIFY(grid%emiss_mosaic)
ENDIF
IF ( ASSOCIATED( grid%embck_mosaic ) ) THEN 
  DEALLOCATE(grid%embck_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11493,&
'frame/module_domain.f: Failed to deallocate grid%embck_mosaic. ')
 endif
  NULLIFY(grid%embck_mosaic)
ENDIF
IF ( ASSOCIATED( grid%znt_mosaic ) ) THEN 
  DEALLOCATE(grid%znt_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11501,&
'frame/module_domain.f: Failed to deallocate grid%znt_mosaic. ')
 endif
  NULLIFY(grid%znt_mosaic)
ENDIF
IF ( ASSOCIATED( grid%z0_mosaic ) ) THEN 
  DEALLOCATE(grid%z0_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11509,&
'frame/module_domain.f: Failed to deallocate grid%z0_mosaic. ')
 endif
  NULLIFY(grid%z0_mosaic)
ENDIF
IF ( ASSOCIATED( grid%lai_mosaic ) ) THEN 
  DEALLOCATE(grid%lai_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11517,&
'frame/module_domain.f: Failed to deallocate grid%lai_mosaic. ')
 endif
  NULLIFY(grid%lai_mosaic)
ENDIF
IF ( ASSOCIATED( grid%rs_mosaic ) ) THEN 
  DEALLOCATE(grid%rs_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11525,&
'frame/module_domain.f: Failed to deallocate grid%rs_mosaic. ')
 endif
  NULLIFY(grid%rs_mosaic)
ENDIF
IF ( ASSOCIATED( grid%hfx_mosaic ) ) THEN 
  DEALLOCATE(grid%hfx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11533,&
'frame/module_domain.f: Failed to deallocate grid%hfx_mosaic. ')
 endif
  NULLIFY(grid%hfx_mosaic)
ENDIF
IF ( ASSOCIATED( grid%qfx_mosaic ) ) THEN 
  DEALLOCATE(grid%qfx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11541,&
'frame/module_domain.f: Failed to deallocate grid%qfx_mosaic. ')
 endif
  NULLIFY(grid%qfx_mosaic)
ENDIF
IF ( ASSOCIATED( grid%lh_mosaic ) ) THEN 
  DEALLOCATE(grid%lh_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11549,&
'frame/module_domain.f: Failed to deallocate grid%lh_mosaic. ')
 endif
  NULLIFY(grid%lh_mosaic)
ENDIF
IF ( ASSOCIATED( grid%grdflx_mosaic ) ) THEN 
  DEALLOCATE(grid%grdflx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11557,&
'frame/module_domain.f: Failed to deallocate grid%grdflx_mosaic. ')
 endif
  NULLIFY(grid%grdflx_mosaic)
ENDIF
IF ( ASSOCIATED( grid%snotime_mosaic ) ) THEN 
  DEALLOCATE(grid%snotime_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11565,&
'frame/module_domain.f: Failed to deallocate grid%snotime_mosaic. ')
 endif
  NULLIFY(grid%snotime_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tr_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tr_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11573,&
'frame/module_domain.f: Failed to deallocate grid%tr_urb2d_mosaic. ')
 endif
  NULLIFY(grid%tr_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tb_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11581,&
'frame/module_domain.f: Failed to deallocate grid%tb_urb2d_mosaic. ')
 endif
  NULLIFY(grid%tb_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tg_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11589,&
'frame/module_domain.f: Failed to deallocate grid%tg_urb2d_mosaic. ')
 endif
  NULLIFY(grid%tg_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11597,&
'frame/module_domain.f: Failed to deallocate grid%tc_urb2d_mosaic. ')
 endif
  NULLIFY(grid%tc_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%ts_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11605,&
'frame/module_domain.f: Failed to deallocate grid%ts_urb2d_mosaic. ')
 endif
  NULLIFY(grid%ts_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%ts_rul2d_mosaic ) ) THEN 
  DEALLOCATE(grid%ts_rul2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11613,&
'frame/module_domain.f: Failed to deallocate grid%ts_rul2d_mosaic. ')
 endif
  NULLIFY(grid%ts_rul2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%qc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11621,&
'frame/module_domain.f: Failed to deallocate grid%qc_urb2d_mosaic. ')
 endif
  NULLIFY(grid%qc_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%uc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11629,&
'frame/module_domain.f: Failed to deallocate grid%uc_urb2d_mosaic. ')
 endif
  NULLIFY(grid%uc_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%trl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11637,&
'frame/module_domain.f: Failed to deallocate grid%trl_urb3d_mosaic. ')
 endif
  NULLIFY(grid%trl_urb3d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11645,&
'frame/module_domain.f: Failed to deallocate grid%tbl_urb3d_mosaic. ')
 endif
  NULLIFY(grid%tbl_urb3d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11653,&
'frame/module_domain.f: Failed to deallocate grid%tgl_urb3d_mosaic. ')
 endif
  NULLIFY(grid%tgl_urb3d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%sh_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11661,&
'frame/module_domain.f: Failed to deallocate grid%sh_urb2d_mosaic. ')
 endif
  NULLIFY(grid%sh_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%lh_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11669,&
'frame/module_domain.f: Failed to deallocate grid%lh_urb2d_mosaic. ')
 endif
  NULLIFY(grid%lh_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%g_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%g_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11677,&
'frame/module_domain.f: Failed to deallocate grid%g_urb2d_mosaic. ')
 endif
  NULLIFY(grid%g_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%rn_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11685,&
'frame/module_domain.f: Failed to deallocate grid%rn_urb2d_mosaic. ')
 endif
  NULLIFY(grid%rn_urb2d_mosaic)
ENDIF
IF ( ASSOCIATED( grid%mosaic_cat_index ) ) THEN 
  DEALLOCATE(grid%mosaic_cat_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11693,&
'frame/module_domain.f: Failed to deallocate grid%mosaic_cat_index. ')
 endif
  NULLIFY(grid%mosaic_cat_index)
ENDIF
IF ( ASSOCIATED( grid%landusef2 ) ) THEN 
  DEALLOCATE(grid%landusef2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11701,&
'frame/module_domain.f: Failed to deallocate grid%landusef2. ')
 endif
  NULLIFY(grid%landusef2)
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11709,&
'frame/module_domain.f: Failed to deallocate grid%mp_restart_state. ')
 endif
  NULLIFY(grid%mp_restart_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11717,&
'frame/module_domain.f: Failed to deallocate grid%tbpvs_state. ')
 endif
  NULLIFY(grid%tbpvs_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11725,&
'frame/module_domain.f: Failed to deallocate grid%tbpvs0_state. ')
 endif
  NULLIFY(grid%tbpvs0_state)
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11733,&
'frame/module_domain.f: Failed to deallocate grid%lu_state. ')
 endif
  NULLIFY(grid%lu_state)
ENDIF
IF ( ASSOCIATED( grid%t_phy ) ) THEN 
  DEALLOCATE(grid%t_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11741,&
'frame/module_domain.f: Failed to deallocate grid%t_phy. ')
 endif
  NULLIFY(grid%t_phy)
ENDIF
IF ( ASSOCIATED( grid%u_phy ) ) THEN 
  DEALLOCATE(grid%u_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11749,&
'frame/module_domain.f: Failed to deallocate grid%u_phy. ')
 endif
  NULLIFY(grid%u_phy)
ENDIF
IF ( ASSOCIATED( grid%v_phy ) ) THEN 
  DEALLOCATE(grid%v_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11757,&
'frame/module_domain.f: Failed to deallocate grid%v_phy. ')
 endif
  NULLIFY(grid%v_phy)
ENDIF
IF ( ASSOCIATED( grid%tmn ) ) THEN 
  DEALLOCATE(grid%tmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11765,&
'frame/module_domain.f: Failed to deallocate grid%tmn. ')
 endif
  NULLIFY(grid%tmn)
ENDIF
IF ( ASSOCIATED( grid%tyr ) ) THEN 
  DEALLOCATE(grid%tyr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11773,&
'frame/module_domain.f: Failed to deallocate grid%tyr. ')
 endif
  NULLIFY(grid%tyr)
ENDIF
IF ( ASSOCIATED( grid%tyra ) ) THEN 
  DEALLOCATE(grid%tyra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11781,&
'frame/module_domain.f: Failed to deallocate grid%tyra. ')
 endif
  NULLIFY(grid%tyra)
ENDIF
IF ( ASSOCIATED( grid%tdly ) ) THEN 
  DEALLOCATE(grid%tdly,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11789,&
'frame/module_domain.f: Failed to deallocate grid%tdly. ')
 endif
  NULLIFY(grid%tdly)
ENDIF
IF ( ASSOCIATED( grid%tlag ) ) THEN 
  DEALLOCATE(grid%tlag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11797,&
'frame/module_domain.f: Failed to deallocate grid%tlag. ')
 endif
  NULLIFY(grid%tlag)
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11805,&
'frame/module_domain.f: Failed to deallocate grid%xland. ')
 endif
  NULLIFY(grid%xland)
ENDIF
IF ( ASSOCIATED( grid%cplmask ) ) THEN 
  DEALLOCATE(grid%cplmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11813,&
'frame/module_domain.f: Failed to deallocate grid%cplmask. ')
 endif
  NULLIFY(grid%cplmask)
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11821,&
'frame/module_domain.f: Failed to deallocate grid%znt. ')
 endif
  NULLIFY(grid%znt)
ENDIF
IF ( ASSOCIATED( grid%ck ) ) THEN 
  DEALLOCATE(grid%ck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11829,&
'frame/module_domain.f: Failed to deallocate grid%ck. ')
 endif
  NULLIFY(grid%ck)
ENDIF
IF ( ASSOCIATED( grid%cka ) ) THEN 
  DEALLOCATE(grid%cka,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11837,&
'frame/module_domain.f: Failed to deallocate grid%cka. ')
 endif
  NULLIFY(grid%cka)
ENDIF
IF ( ASSOCIATED( grid%cd ) ) THEN 
  DEALLOCATE(grid%cd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11845,&
'frame/module_domain.f: Failed to deallocate grid%cd. ')
 endif
  NULLIFY(grid%cd)
ENDIF
IF ( ASSOCIATED( grid%cda ) ) THEN 
  DEALLOCATE(grid%cda,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11853,&
'frame/module_domain.f: Failed to deallocate grid%cda. ')
 endif
  NULLIFY(grid%cda)
ENDIF
IF ( ASSOCIATED( grid%ust ) ) THEN 
  DEALLOCATE(grid%ust,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11861,&
'frame/module_domain.f: Failed to deallocate grid%ust. ')
 endif
  NULLIFY(grid%ust)
ENDIF
IF ( ASSOCIATED( grid%ustm ) ) THEN 
  DEALLOCATE(grid%ustm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11869,&
'frame/module_domain.f: Failed to deallocate grid%ustm. ')
 endif
  NULLIFY(grid%ustm)
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11877,&
'frame/module_domain.f: Failed to deallocate grid%rmol. ')
 endif
  NULLIFY(grid%rmol)
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11885,&
'frame/module_domain.f: Failed to deallocate grid%mol. ')
 endif
  NULLIFY(grid%mol)
ENDIF
IF ( ASSOCIATED( grid%pblh ) ) THEN 
  DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11893,&
'frame/module_domain.f: Failed to deallocate grid%pblh. ')
 endif
  NULLIFY(grid%pblh)
ENDIF
IF ( ASSOCIATED( grid%capg ) ) THEN 
  DEALLOCATE(grid%capg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11901,&
'frame/module_domain.f: Failed to deallocate grid%capg. ')
 endif
  NULLIFY(grid%capg)
ENDIF
IF ( ASSOCIATED( grid%thc ) ) THEN 
  DEALLOCATE(grid%thc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11909,&
'frame/module_domain.f: Failed to deallocate grid%thc. ')
 endif
  NULLIFY(grid%thc)
ENDIF
IF ( ASSOCIATED( grid%hfx ) ) THEN 
  DEALLOCATE(grid%hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11917,&
'frame/module_domain.f: Failed to deallocate grid%hfx. ')
 endif
  NULLIFY(grid%hfx)
ENDIF
IF ( ASSOCIATED( grid%qfx ) ) THEN 
  DEALLOCATE(grid%qfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11925,&
'frame/module_domain.f: Failed to deallocate grid%qfx. ')
 endif
  NULLIFY(grid%qfx)
ENDIF
IF ( ASSOCIATED( grid%lh ) ) THEN 
  DEALLOCATE(grid%lh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11933,&
'frame/module_domain.f: Failed to deallocate grid%lh. ')
 endif
  NULLIFY(grid%lh)
ENDIF
IF ( ASSOCIATED( grid%achfx ) ) THEN 
  DEALLOCATE(grid%achfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11941,&
'frame/module_domain.f: Failed to deallocate grid%achfx. ')
 endif
  NULLIFY(grid%achfx)
ENDIF
IF ( ASSOCIATED( grid%wstar ) ) THEN 
  DEALLOCATE(grid%wstar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11949,&
'frame/module_domain.f: Failed to deallocate grid%wstar. ')
 endif
  NULLIFY(grid%wstar)
ENDIF
IF ( ASSOCIATED( grid%aclhf ) ) THEN 
  DEALLOCATE(grid%aclhf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11957,&
'frame/module_domain.f: Failed to deallocate grid%aclhf. ')
 endif
  NULLIFY(grid%aclhf)
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11965,&
'frame/module_domain.f: Failed to deallocate grid%flhc. ')
 endif
  NULLIFY(grid%flhc)
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11973,&
'frame/module_domain.f: Failed to deallocate grid%flqc. ')
 endif
  NULLIFY(grid%flqc)
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11981,&
'frame/module_domain.f: Failed to deallocate grid%qsg. ')
 endif
  NULLIFY(grid%qsg)
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11989,&
'frame/module_domain.f: Failed to deallocate grid%qvg. ')
 endif
  NULLIFY(grid%qvg)
ENDIF
IF ( ASSOCIATED( grid%dfi_qvg ) ) THEN 
  DEALLOCATE(grid%dfi_qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11997,&
'frame/module_domain.f: Failed to deallocate grid%dfi_qvg. ')
 endif
  NULLIFY(grid%dfi_qvg)
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12005,&
'frame/module_domain.f: Failed to deallocate grid%qcg. ')
 endif
  NULLIFY(grid%qcg)
ENDIF
IF ( ASSOCIATED( grid%dew ) ) THEN 
  DEALLOCATE(grid%dew,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12013,&
'frame/module_domain.f: Failed to deallocate grid%dew. ')
 endif
  NULLIFY(grid%dew)
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12021,&
'frame/module_domain.f: Failed to deallocate grid%soilt1. ')
 endif
  NULLIFY(grid%soilt1)
ENDIF
IF ( ASSOCIATED( grid%dfi_soilt1 ) ) THEN 
  DEALLOCATE(grid%dfi_soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12029,&
'frame/module_domain.f: Failed to deallocate grid%dfi_soilt1. ')
 endif
  NULLIFY(grid%dfi_soilt1)
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12037,&
'frame/module_domain.f: Failed to deallocate grid%tsnav. ')
 endif
  NULLIFY(grid%tsnav)
ENDIF
IF ( ASSOCIATED( grid%dfi_tsnav ) ) THEN 
  DEALLOCATE(grid%dfi_tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12045,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tsnav. ')
 endif
  NULLIFY(grid%dfi_tsnav)
ENDIF
IF ( ASSOCIATED( grid%regime ) ) THEN 
  DEALLOCATE(grid%regime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12053,&
'frame/module_domain.f: Failed to deallocate grid%regime. ')
 endif
  NULLIFY(grid%regime)
ENDIF
IF ( ASSOCIATED( grid%snowc ) ) THEN 
  DEALLOCATE(grid%snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12061,&
'frame/module_domain.f: Failed to deallocate grid%snowc. ')
 endif
  NULLIFY(grid%snowc)
ENDIF
IF ( ASSOCIATED( grid%dfi_snowc ) ) THEN 
  DEALLOCATE(grid%dfi_snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12069,&
'frame/module_domain.f: Failed to deallocate grid%dfi_snowc. ')
 endif
  NULLIFY(grid%dfi_snowc)
ENDIF
IF ( ASSOCIATED( grid%mavail ) ) THEN 
  DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12077,&
'frame/module_domain.f: Failed to deallocate grid%mavail. ')
 endif
  NULLIFY(grid%mavail)
ENDIF
IF ( ASSOCIATED( grid%tkesfcf ) ) THEN 
  DEALLOCATE(grid%tkesfcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12085,&
'frame/module_domain.f: Failed to deallocate grid%tkesfcf. ')
 endif
  NULLIFY(grid%tkesfcf)
ENDIF
IF ( ASSOCIATED( grid%sr ) ) THEN 
  DEALLOCATE(grid%sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12093,&
'frame/module_domain.f: Failed to deallocate grid%sr. ')
 endif
  NULLIFY(grid%sr)
ENDIF
IF ( ASSOCIATED( grid%potevp ) ) THEN 
  DEALLOCATE(grid%potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12101,&
'frame/module_domain.f: Failed to deallocate grid%potevp. ')
 endif
  NULLIFY(grid%potevp)
ENDIF
IF ( ASSOCIATED( grid%snopcx ) ) THEN 
  DEALLOCATE(grid%snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12109,&
'frame/module_domain.f: Failed to deallocate grid%snopcx. ')
 endif
  NULLIFY(grid%snopcx)
ENDIF
IF ( ASSOCIATED( grid%soiltb ) ) THEN 
  DEALLOCATE(grid%soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12117,&
'frame/module_domain.f: Failed to deallocate grid%soiltb. ')
 endif
  NULLIFY(grid%soiltb)
ENDIF
IF ( ASSOCIATED( grid%taucldi ) ) THEN 
  DEALLOCATE(grid%taucldi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12125,&
'frame/module_domain.f: Failed to deallocate grid%taucldi. ')
 endif
  NULLIFY(grid%taucldi)
ENDIF
IF ( ASSOCIATED( grid%taucldc ) ) THEN 
  DEALLOCATE(grid%taucldc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12133,&
'frame/module_domain.f: Failed to deallocate grid%taucldc. ')
 endif
  NULLIFY(grid%taucldc)
ENDIF
IF ( ASSOCIATED( grid%defor11 ) ) THEN 
  DEALLOCATE(grid%defor11,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12141,&
'frame/module_domain.f: Failed to deallocate grid%defor11. ')
 endif
  NULLIFY(grid%defor11)
ENDIF
IF ( ASSOCIATED( grid%defor22 ) ) THEN 
  DEALLOCATE(grid%defor22,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12149,&
'frame/module_domain.f: Failed to deallocate grid%defor22. ')
 endif
  NULLIFY(grid%defor22)
ENDIF
IF ( ASSOCIATED( grid%defor12 ) ) THEN 
  DEALLOCATE(grid%defor12,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12157,&
'frame/module_domain.f: Failed to deallocate grid%defor12. ')
 endif
  NULLIFY(grid%defor12)
ENDIF
IF ( ASSOCIATED( grid%defor33 ) ) THEN 
  DEALLOCATE(grid%defor33,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12165,&
'frame/module_domain.f: Failed to deallocate grid%defor33. ')
 endif
  NULLIFY(grid%defor33)
ENDIF
IF ( ASSOCIATED( grid%defor13 ) ) THEN 
  DEALLOCATE(grid%defor13,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12173,&
'frame/module_domain.f: Failed to deallocate grid%defor13. ')
 endif
  NULLIFY(grid%defor13)
ENDIF
IF ( ASSOCIATED( grid%defor23 ) ) THEN 
  DEALLOCATE(grid%defor23,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12181,&
'frame/module_domain.f: Failed to deallocate grid%defor23. ')
 endif
  NULLIFY(grid%defor23)
ENDIF
IF ( ASSOCIATED( grid%xkmv ) ) THEN 
  DEALLOCATE(grid%xkmv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12189,&
'frame/module_domain.f: Failed to deallocate grid%xkmv. ')
 endif
  NULLIFY(grid%xkmv)
ENDIF
IF ( ASSOCIATED( grid%xkmh ) ) THEN 
  DEALLOCATE(grid%xkmh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12197,&
'frame/module_domain.f: Failed to deallocate grid%xkmh. ')
 endif
  NULLIFY(grid%xkmh)
ENDIF
IF ( ASSOCIATED( grid%xkhv ) ) THEN 
  DEALLOCATE(grid%xkhv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12205,&
'frame/module_domain.f: Failed to deallocate grid%xkhv. ')
 endif
  NULLIFY(grid%xkhv)
ENDIF
IF ( ASSOCIATED( grid%xkhh ) ) THEN 
  DEALLOCATE(grid%xkhh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12213,&
'frame/module_domain.f: Failed to deallocate grid%xkhh. ')
 endif
  NULLIFY(grid%xkhh)
ENDIF
IF ( ASSOCIATED( grid%div ) ) THEN 
  DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12221,&
'frame/module_domain.f: Failed to deallocate grid%div. ')
 endif
  NULLIFY(grid%div)
ENDIF
IF ( ASSOCIATED( grid%bn2 ) ) THEN 
  DEALLOCATE(grid%bn2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12229,&
'frame/module_domain.f: Failed to deallocate grid%bn2. ')
 endif
  NULLIFY(grid%bn2)
ENDIF
IF ( ASSOCIATED( grid%rundgdten ) ) THEN 
  DEALLOCATE(grid%rundgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12237,&
'frame/module_domain.f: Failed to deallocate grid%rundgdten. ')
 endif
  NULLIFY(grid%rundgdten)
ENDIF
IF ( ASSOCIATED( grid%rvndgdten ) ) THEN 
  DEALLOCATE(grid%rvndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12245,&
'frame/module_domain.f: Failed to deallocate grid%rvndgdten. ')
 endif
  NULLIFY(grid%rvndgdten)
ENDIF
IF ( ASSOCIATED( grid%rthndgdten ) ) THEN 
  DEALLOCATE(grid%rthndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12253,&
'frame/module_domain.f: Failed to deallocate grid%rthndgdten. ')
 endif
  NULLIFY(grid%rthndgdten)
ENDIF
IF ( ASSOCIATED( grid%rphndgdten ) ) THEN 
  DEALLOCATE(grid%rphndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12261,&
'frame/module_domain.f: Failed to deallocate grid%rphndgdten. ')
 endif
  NULLIFY(grid%rphndgdten)
ENDIF
IF ( ASSOCIATED( grid%rqvndgdten ) ) THEN 
  DEALLOCATE(grid%rqvndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12269,&
'frame/module_domain.f: Failed to deallocate grid%rqvndgdten. ')
 endif
  NULLIFY(grid%rqvndgdten)
ENDIF
IF ( ASSOCIATED( grid%rmundgdten ) ) THEN 
  DEALLOCATE(grid%rmundgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12277,&
'frame/module_domain.f: Failed to deallocate grid%rmundgdten. ')
 endif
  NULLIFY(grid%rmundgdten)
ENDIF
IF ( ASSOCIATED( grid%fdda3d ) ) THEN 
  DEALLOCATE(grid%fdda3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12285,&
'frame/module_domain.f: Failed to deallocate grid%fdda3d. ')
 endif
  NULLIFY(grid%fdda3d)
ENDIF
IF ( ASSOCIATED( grid%fdda2d ) ) THEN 
  DEALLOCATE(grid%fdda2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12293,&
'frame/module_domain.f: Failed to deallocate grid%fdda2d. ')
 endif
  NULLIFY(grid%fdda2d)
ENDIF
IF ( ASSOCIATED( grid%u10_ndg_old ) ) THEN 
  DEALLOCATE(grid%u10_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12301,&
'frame/module_domain.f: Failed to deallocate grid%u10_ndg_old. ')
 endif
  NULLIFY(grid%u10_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%u10_ndg_new ) ) THEN 
  DEALLOCATE(grid%u10_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12309,&
'frame/module_domain.f: Failed to deallocate grid%u10_ndg_new. ')
 endif
  NULLIFY(grid%u10_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%v10_ndg_old ) ) THEN 
  DEALLOCATE(grid%v10_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12317,&
'frame/module_domain.f: Failed to deallocate grid%v10_ndg_old. ')
 endif
  NULLIFY(grid%v10_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%v10_ndg_new ) ) THEN 
  DEALLOCATE(grid%v10_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12325,&
'frame/module_domain.f: Failed to deallocate grid%v10_ndg_new. ')
 endif
  NULLIFY(grid%v10_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%t2_ndg_old ) ) THEN 
  DEALLOCATE(grid%t2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12333,&
'frame/module_domain.f: Failed to deallocate grid%t2_ndg_old. ')
 endif
  NULLIFY(grid%t2_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%t2_ndg_new ) ) THEN 
  DEALLOCATE(grid%t2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12341,&
'frame/module_domain.f: Failed to deallocate grid%t2_ndg_new. ')
 endif
  NULLIFY(grid%t2_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%th2_ndg_old ) ) THEN 
  DEALLOCATE(grid%th2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12349,&
'frame/module_domain.f: Failed to deallocate grid%th2_ndg_old. ')
 endif
  NULLIFY(grid%th2_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%th2_ndg_new ) ) THEN 
  DEALLOCATE(grid%th2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12357,&
'frame/module_domain.f: Failed to deallocate grid%th2_ndg_new. ')
 endif
  NULLIFY(grid%th2_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%q2_ndg_old ) ) THEN 
  DEALLOCATE(grid%q2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12365,&
'frame/module_domain.f: Failed to deallocate grid%q2_ndg_old. ')
 endif
  NULLIFY(grid%q2_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%q2_ndg_new ) ) THEN 
  DEALLOCATE(grid%q2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12373,&
'frame/module_domain.f: Failed to deallocate grid%q2_ndg_new. ')
 endif
  NULLIFY(grid%q2_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%rh_ndg_old ) ) THEN 
  DEALLOCATE(grid%rh_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12381,&
'frame/module_domain.f: Failed to deallocate grid%rh_ndg_old. ')
 endif
  NULLIFY(grid%rh_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%rh_ndg_new ) ) THEN 
  DEALLOCATE(grid%rh_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12389,&
'frame/module_domain.f: Failed to deallocate grid%rh_ndg_new. ')
 endif
  NULLIFY(grid%rh_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%psl_ndg_old ) ) THEN 
  DEALLOCATE(grid%psl_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12397,&
'frame/module_domain.f: Failed to deallocate grid%psl_ndg_old. ')
 endif
  NULLIFY(grid%psl_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%psl_ndg_new ) ) THEN 
  DEALLOCATE(grid%psl_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12405,&
'frame/module_domain.f: Failed to deallocate grid%psl_ndg_new. ')
 endif
  NULLIFY(grid%psl_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%ps_ndg_old ) ) THEN 
  DEALLOCATE(grid%ps_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12413,&
'frame/module_domain.f: Failed to deallocate grid%ps_ndg_old. ')
 endif
  NULLIFY(grid%ps_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%ps_ndg_new ) ) THEN 
  DEALLOCATE(grid%ps_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12421,&
'frame/module_domain.f: Failed to deallocate grid%ps_ndg_new. ')
 endif
  NULLIFY(grid%ps_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%tob_ndg_old ) ) THEN 
  DEALLOCATE(grid%tob_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12429,&
'frame/module_domain.f: Failed to deallocate grid%tob_ndg_old. ')
 endif
  NULLIFY(grid%tob_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%odis_ndg_old ) ) THEN 
  DEALLOCATE(grid%odis_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12437,&
'frame/module_domain.f: Failed to deallocate grid%odis_ndg_old. ')
 endif
  NULLIFY(grid%odis_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%tob_ndg_new ) ) THEN 
  DEALLOCATE(grid%tob_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12445,&
'frame/module_domain.f: Failed to deallocate grid%tob_ndg_new. ')
 endif
  NULLIFY(grid%tob_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%odis_ndg_new ) ) THEN 
  DEALLOCATE(grid%odis_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12453,&
'frame/module_domain.f: Failed to deallocate grid%odis_ndg_new. ')
 endif
  NULLIFY(grid%odis_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%sn_ndg_new ) ) THEN 
  DEALLOCATE(grid%sn_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12461,&
'frame/module_domain.f: Failed to deallocate grid%sn_ndg_new. ')
 endif
  NULLIFY(grid%sn_ndg_new)
ENDIF
IF ( ASSOCIATED( grid%sn_ndg_old ) ) THEN 
  DEALLOCATE(grid%sn_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12469,&
'frame/module_domain.f: Failed to deallocate grid%sn_ndg_old. ')
 endif
  NULLIFY(grid%sn_ndg_old)
ENDIF
IF ( ASSOCIATED( grid%sda_hfx ) ) THEN 
  DEALLOCATE(grid%sda_hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12477,&
'frame/module_domain.f: Failed to deallocate grid%sda_hfx. ')
 endif
  NULLIFY(grid%sda_hfx)
ENDIF
IF ( ASSOCIATED( grid%sda_qfx ) ) THEN 
  DEALLOCATE(grid%sda_qfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12485,&
'frame/module_domain.f: Failed to deallocate grid%sda_qfx. ')
 endif
  NULLIFY(grid%sda_qfx)
ENDIF
IF ( ASSOCIATED( grid%qnorm ) ) THEN 
  DEALLOCATE(grid%qnorm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12493,&
'frame/module_domain.f: Failed to deallocate grid%qnorm. ')
 endif
  NULLIFY(grid%qnorm)
ENDIF
IF ( ASSOCIATED( grid%hfx_both ) ) THEN 
  DEALLOCATE(grid%hfx_both,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12501,&
'frame/module_domain.f: Failed to deallocate grid%hfx_both. ')
 endif
  NULLIFY(grid%hfx_both)
ENDIF
IF ( ASSOCIATED( grid%qfx_both ) ) THEN 
  DEALLOCATE(grid%qfx_both,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12509,&
'frame/module_domain.f: Failed to deallocate grid%qfx_both. ')
 endif
  NULLIFY(grid%qfx_both)
ENDIF
IF ( ASSOCIATED( grid%hfx_fdda ) ) THEN 
  DEALLOCATE(grid%hfx_fdda,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12517,&
'frame/module_domain.f: Failed to deallocate grid%hfx_fdda. ')
 endif
  NULLIFY(grid%hfx_fdda)
ENDIF
IF ( ASSOCIATED( grid%abstot ) ) THEN 
  DEALLOCATE(grid%abstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12525,&
'frame/module_domain.f: Failed to deallocate grid%abstot. ')
 endif
  NULLIFY(grid%abstot)
ENDIF
IF ( ASSOCIATED( grid%absnxt ) ) THEN 
  DEALLOCATE(grid%absnxt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12533,&
'frame/module_domain.f: Failed to deallocate grid%absnxt. ')
 endif
  NULLIFY(grid%absnxt)
ENDIF
IF ( ASSOCIATED( grid%emstot ) ) THEN 
  DEALLOCATE(grid%emstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12541,&
'frame/module_domain.f: Failed to deallocate grid%emstot. ')
 endif
  NULLIFY(grid%emstot)
ENDIF
IF ( ASSOCIATED( grid%dpsdt ) ) THEN 
  DEALLOCATE(grid%dpsdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12549,&
'frame/module_domain.f: Failed to deallocate grid%dpsdt. ')
 endif
  NULLIFY(grid%dpsdt)
ENDIF
IF ( ASSOCIATED( grid%dmudt ) ) THEN 
  DEALLOCATE(grid%dmudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12557,&
'frame/module_domain.f: Failed to deallocate grid%dmudt. ')
 endif
  NULLIFY(grid%dmudt)
ENDIF
IF ( ASSOCIATED( grid%pk1m ) ) THEN 
  DEALLOCATE(grid%pk1m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12565,&
'frame/module_domain.f: Failed to deallocate grid%pk1m. ')
 endif
  NULLIFY(grid%pk1m)
ENDIF
IF ( ASSOCIATED( grid%mu_2m ) ) THEN 
  DEALLOCATE(grid%mu_2m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12573,&
'frame/module_domain.f: Failed to deallocate grid%mu_2m. ')
 endif
  NULLIFY(grid%mu_2m)
ENDIF
IF ( ASSOCIATED( grid%wspd10max ) ) THEN 
  DEALLOCATE(grid%wspd10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12581,&
'frame/module_domain.f: Failed to deallocate grid%wspd10max. ')
 endif
  NULLIFY(grid%wspd10max)
ENDIF
IF ( ASSOCIATED( grid%w_up_max ) ) THEN 
  DEALLOCATE(grid%w_up_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12589,&
'frame/module_domain.f: Failed to deallocate grid%w_up_max. ')
 endif
  NULLIFY(grid%w_up_max)
ENDIF
IF ( ASSOCIATED( grid%w_dn_max ) ) THEN 
  DEALLOCATE(grid%w_dn_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12597,&
'frame/module_domain.f: Failed to deallocate grid%w_dn_max. ')
 endif
  NULLIFY(grid%w_dn_max)
ENDIF
IF ( ASSOCIATED( grid%refd_max ) ) THEN 
  DEALLOCATE(grid%refd_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12605,&
'frame/module_domain.f: Failed to deallocate grid%refd_max. ')
 endif
  NULLIFY(grid%refd_max)
ENDIF
IF ( ASSOCIATED( grid%up_heli_max ) ) THEN 
  DEALLOCATE(grid%up_heli_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12613,&
'frame/module_domain.f: Failed to deallocate grid%up_heli_max. ')
 endif
  NULLIFY(grid%up_heli_max)
ENDIF
IF ( ASSOCIATED( grid%w_mean ) ) THEN 
  DEALLOCATE(grid%w_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12621,&
'frame/module_domain.f: Failed to deallocate grid%w_mean. ')
 endif
  NULLIFY(grid%w_mean)
ENDIF
IF ( ASSOCIATED( grid%grpl_max ) ) THEN 
  DEALLOCATE(grid%grpl_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12629,&
'frame/module_domain.f: Failed to deallocate grid%grpl_max. ')
 endif
  NULLIFY(grid%grpl_max)
ENDIF
IF ( ASSOCIATED( grid%uh ) ) THEN 
  DEALLOCATE(grid%uh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12637,&
'frame/module_domain.f: Failed to deallocate grid%uh. ')
 endif
  NULLIFY(grid%uh)
ENDIF
IF ( ASSOCIATED( grid%w_colmean ) ) THEN 
  DEALLOCATE(grid%w_colmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12645,&
'frame/module_domain.f: Failed to deallocate grid%w_colmean. ')
 endif
  NULLIFY(grid%w_colmean)
ENDIF
IF ( ASSOCIATED( grid%numcolpts ) ) THEN 
  DEALLOCATE(grid%numcolpts,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12653,&
'frame/module_domain.f: Failed to deallocate grid%numcolpts. ')
 endif
  NULLIFY(grid%numcolpts)
ENDIF
IF ( ASSOCIATED( grid%grpl_colint ) ) THEN 
  DEALLOCATE(grid%grpl_colint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12661,&
'frame/module_domain.f: Failed to deallocate grid%grpl_colint. ')
 endif
  NULLIFY(grid%grpl_colint)
ENDIF
IF ( ASSOCIATED( grid%hail_maxk1 ) ) THEN 
  DEALLOCATE(grid%hail_maxk1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12669,&
'frame/module_domain.f: Failed to deallocate grid%hail_maxk1. ')
 endif
  NULLIFY(grid%hail_maxk1)
ENDIF
IF ( ASSOCIATED( grid%hail_max2d ) ) THEN 
  DEALLOCATE(grid%hail_max2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12677,&
'frame/module_domain.f: Failed to deallocate grid%hail_max2d. ')
 endif
  NULLIFY(grid%hail_max2d)
ENDIF
IF ( ASSOCIATED( grid%prec_acc_c ) ) THEN 
  DEALLOCATE(grid%prec_acc_c,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12685,&
'frame/module_domain.f: Failed to deallocate grid%prec_acc_c. ')
 endif
  NULLIFY(grid%prec_acc_c)
ENDIF
IF ( ASSOCIATED( grid%prec_acc_nc ) ) THEN 
  DEALLOCATE(grid%prec_acc_nc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12693,&
'frame/module_domain.f: Failed to deallocate grid%prec_acc_nc. ')
 endif
  NULLIFY(grid%prec_acc_nc)
ENDIF
IF ( ASSOCIATED( grid%snow_acc_nc ) ) THEN 
  DEALLOCATE(grid%snow_acc_nc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12701,&
'frame/module_domain.f: Failed to deallocate grid%snow_acc_nc. ')
 endif
  NULLIFY(grid%snow_acc_nc)
ENDIF
IF ( ASSOCIATED( grid%advh_t ) ) THEN 
  DEALLOCATE(grid%advh_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12709,&
'frame/module_domain.f: Failed to deallocate grid%advh_t. ')
 endif
  NULLIFY(grid%advh_t)
ENDIF
IF ( ASSOCIATED( grid%advz_t ) ) THEN 
  DEALLOCATE(grid%advz_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12717,&
'frame/module_domain.f: Failed to deallocate grid%advz_t. ')
 endif
  NULLIFY(grid%advz_t)
ENDIF
IF ( ASSOCIATED( grid%tml ) ) THEN 
  DEALLOCATE(grid%tml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12725,&
'frame/module_domain.f: Failed to deallocate grid%tml. ')
 endif
  NULLIFY(grid%tml)
ENDIF
IF ( ASSOCIATED( grid%t0ml ) ) THEN 
  DEALLOCATE(grid%t0ml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12733,&
'frame/module_domain.f: Failed to deallocate grid%t0ml. ')
 endif
  NULLIFY(grid%t0ml)
ENDIF
IF ( ASSOCIATED( grid%hml ) ) THEN 
  DEALLOCATE(grid%hml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12741,&
'frame/module_domain.f: Failed to deallocate grid%hml. ')
 endif
  NULLIFY(grid%hml)
ENDIF
IF ( ASSOCIATED( grid%h0ml ) ) THEN 
  DEALLOCATE(grid%h0ml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12749,&
'frame/module_domain.f: Failed to deallocate grid%h0ml. ')
 endif
  NULLIFY(grid%h0ml)
ENDIF
IF ( ASSOCIATED( grid%huml ) ) THEN 
  DEALLOCATE(grid%huml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12757,&
'frame/module_domain.f: Failed to deallocate grid%huml. ')
 endif
  NULLIFY(grid%huml)
ENDIF
IF ( ASSOCIATED( grid%hvml ) ) THEN 
  DEALLOCATE(grid%hvml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12765,&
'frame/module_domain.f: Failed to deallocate grid%hvml. ')
 endif
  NULLIFY(grid%hvml)
ENDIF
IF ( ASSOCIATED( grid%tmoml ) ) THEN 
  DEALLOCATE(grid%tmoml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12773,&
'frame/module_domain.f: Failed to deallocate grid%tmoml. ')
 endif
  NULLIFY(grid%tmoml)
ENDIF
IF ( ASSOCIATED( grid%track_z ) ) THEN 
  DEALLOCATE(grid%track_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12781,&
'frame/module_domain.f: Failed to deallocate grid%track_z. ')
 endif
  NULLIFY(grid%track_z)
ENDIF
IF ( ASSOCIATED( grid%track_t ) ) THEN 
  DEALLOCATE(grid%track_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12789,&
'frame/module_domain.f: Failed to deallocate grid%track_t. ')
 endif
  NULLIFY(grid%track_t)
ENDIF
IF ( ASSOCIATED( grid%track_p ) ) THEN 
  DEALLOCATE(grid%track_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12797,&
'frame/module_domain.f: Failed to deallocate grid%track_p. ')
 endif
  NULLIFY(grid%track_p)
ENDIF
IF ( ASSOCIATED( grid%track_u ) ) THEN 
  DEALLOCATE(grid%track_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12805,&
'frame/module_domain.f: Failed to deallocate grid%track_u. ')
 endif
  NULLIFY(grid%track_u)
ENDIF
IF ( ASSOCIATED( grid%track_v ) ) THEN 
  DEALLOCATE(grid%track_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12813,&
'frame/module_domain.f: Failed to deallocate grid%track_v. ')
 endif
  NULLIFY(grid%track_v)
ENDIF
IF ( ASSOCIATED( grid%track_w ) ) THEN 
  DEALLOCATE(grid%track_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12821,&
'frame/module_domain.f: Failed to deallocate grid%track_w. ')
 endif
  NULLIFY(grid%track_w)
ENDIF
IF ( ASSOCIATED( grid%track_rh ) ) THEN 
  DEALLOCATE(grid%track_rh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12829,&
'frame/module_domain.f: Failed to deallocate grid%track_rh. ')
 endif
  NULLIFY(grid%track_rh)
ENDIF
IF ( ASSOCIATED( grid%track_alt ) ) THEN 
  DEALLOCATE(grid%track_alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12837,&
'frame/module_domain.f: Failed to deallocate grid%track_alt. ')
 endif
  NULLIFY(grid%track_alt)
ENDIF
IF ( ASSOCIATED( grid%track_ele ) ) THEN 
  DEALLOCATE(grid%track_ele,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12845,&
'frame/module_domain.f: Failed to deallocate grid%track_ele. ')
 endif
  NULLIFY(grid%track_ele)
ENDIF
IF ( ASSOCIATED( grid%track_aircraft ) ) THEN 
  DEALLOCATE(grid%track_aircraft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12853,&
'frame/module_domain.f: Failed to deallocate grid%track_aircraft. ')
 endif
  NULLIFY(grid%track_aircraft)
ENDIF
IF ( ASSOCIATED( grid%track_qcloud ) ) THEN 
  DEALLOCATE(grid%track_qcloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12861,&
'frame/module_domain.f: Failed to deallocate grid%track_qcloud. ')
 endif
  NULLIFY(grid%track_qcloud)
ENDIF
IF ( ASSOCIATED( grid%track_qrain ) ) THEN 
  DEALLOCATE(grid%track_qrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12869,&
'frame/module_domain.f: Failed to deallocate grid%track_qrain. ')
 endif
  NULLIFY(grid%track_qrain)
ENDIF
IF ( ASSOCIATED( grid%track_qice ) ) THEN 
  DEALLOCATE(grid%track_qice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12877,&
'frame/module_domain.f: Failed to deallocate grid%track_qice. ')
 endif
  NULLIFY(grid%track_qice)
ENDIF
IF ( ASSOCIATED( grid%track_qsnow ) ) THEN 
  DEALLOCATE(grid%track_qsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12885,&
'frame/module_domain.f: Failed to deallocate grid%track_qsnow. ')
 endif
  NULLIFY(grid%track_qsnow)
ENDIF
IF ( ASSOCIATED( grid%track_qgraup ) ) THEN 
  DEALLOCATE(grid%track_qgraup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12893,&
'frame/module_domain.f: Failed to deallocate grid%track_qgraup. ')
 endif
  NULLIFY(grid%track_qgraup)
ENDIF
IF ( ASSOCIATED( grid%track_qvapor ) ) THEN 
  DEALLOCATE(grid%track_qvapor,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12901,&
'frame/module_domain.f: Failed to deallocate grid%track_qvapor. ')
 endif
  NULLIFY(grid%track_qvapor)
ENDIF
IF ( ASSOCIATED( grid%brtemp ) ) THEN 
  DEALLOCATE(grid%brtemp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12909,&
'frame/module_domain.f: Failed to deallocate grid%brtemp. ')
 endif
  NULLIFY(grid%brtemp)
ENDIF
IF ( ASSOCIATED( grid%cldmask ) ) THEN 
  DEALLOCATE(grid%cldmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12917,&
'frame/module_domain.f: Failed to deallocate grid%cldmask. ')
 endif
  NULLIFY(grid%cldmask)
ENDIF
IF ( ASSOCIATED( grid%cldtopz ) ) THEN 
  DEALLOCATE(grid%cldtopz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12925,&
'frame/module_domain.f: Failed to deallocate grid%cldtopz. ')
 endif
  NULLIFY(grid%cldtopz)
ENDIF
IF ( ASSOCIATED( grid%cldbasez ) ) THEN 
  DEALLOCATE(grid%cldbasez,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12933,&
'frame/module_domain.f: Failed to deallocate grid%cldbasez. ')
 endif
  NULLIFY(grid%cldbasez)
ENDIF
IF ( ASSOCIATED( grid%athmpten ) ) THEN 
  DEALLOCATE(grid%athmpten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12941,&
'frame/module_domain.f: Failed to deallocate grid%athmpten. ')
 endif
  NULLIFY(grid%athmpten)
ENDIF
IF ( ASSOCIATED( grid%aqvmpten ) ) THEN 
  DEALLOCATE(grid%aqvmpten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12949,&
'frame/module_domain.f: Failed to deallocate grid%aqvmpten. ')
 endif
  NULLIFY(grid%aqvmpten)
ENDIF
IF ( ASSOCIATED( grid%athcuten ) ) THEN 
  DEALLOCATE(grid%athcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12957,&
'frame/module_domain.f: Failed to deallocate grid%athcuten. ')
 endif
  NULLIFY(grid%athcuten)
ENDIF
IF ( ASSOCIATED( grid%aqvcuten ) ) THEN 
  DEALLOCATE(grid%aqvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12965,&
'frame/module_domain.f: Failed to deallocate grid%aqvcuten. ')
 endif
  NULLIFY(grid%aqvcuten)
ENDIF
IF ( ASSOCIATED( grid%aucuten ) ) THEN 
  DEALLOCATE(grid%aucuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12973,&
'frame/module_domain.f: Failed to deallocate grid%aucuten. ')
 endif
  NULLIFY(grid%aucuten)
ENDIF
IF ( ASSOCIATED( grid%avcuten ) ) THEN 
  DEALLOCATE(grid%avcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12981,&
'frame/module_domain.f: Failed to deallocate grid%avcuten. ')
 endif
  NULLIFY(grid%avcuten)
ENDIF
IF ( ASSOCIATED( grid%athshten ) ) THEN 
  DEALLOCATE(grid%athshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12989,&
'frame/module_domain.f: Failed to deallocate grid%athshten. ')
 endif
  NULLIFY(grid%athshten)
ENDIF
IF ( ASSOCIATED( grid%aqvshten ) ) THEN 
  DEALLOCATE(grid%aqvshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12997,&
'frame/module_domain.f: Failed to deallocate grid%aqvshten. ')
 endif
  NULLIFY(grid%aqvshten)
ENDIF
IF ( ASSOCIATED( grid%aushten ) ) THEN 
  DEALLOCATE(grid%aushten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13005,&
'frame/module_domain.f: Failed to deallocate grid%aushten. ')
 endif
  NULLIFY(grid%aushten)
ENDIF
IF ( ASSOCIATED( grid%avshten ) ) THEN 
  DEALLOCATE(grid%avshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13013,&
'frame/module_domain.f: Failed to deallocate grid%avshten. ')
 endif
  NULLIFY(grid%avshten)
ENDIF
IF ( ASSOCIATED( grid%athblten ) ) THEN 
  DEALLOCATE(grid%athblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13021,&
'frame/module_domain.f: Failed to deallocate grid%athblten. ')
 endif
  NULLIFY(grid%athblten)
ENDIF
IF ( ASSOCIATED( grid%aqvblten ) ) THEN 
  DEALLOCATE(grid%aqvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13029,&
'frame/module_domain.f: Failed to deallocate grid%aqvblten. ')
 endif
  NULLIFY(grid%aqvblten)
ENDIF
IF ( ASSOCIATED( grid%aublten ) ) THEN 
  DEALLOCATE(grid%aublten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13037,&
'frame/module_domain.f: Failed to deallocate grid%aublten. ')
 endif
  NULLIFY(grid%aublten)
ENDIF
IF ( ASSOCIATED( grid%avblten ) ) THEN 
  DEALLOCATE(grid%avblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13045,&
'frame/module_domain.f: Failed to deallocate grid%avblten. ')
 endif
  NULLIFY(grid%avblten)
ENDIF
IF ( ASSOCIATED( grid%athratenlw ) ) THEN 
  DEALLOCATE(grid%athratenlw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13053,&
'frame/module_domain.f: Failed to deallocate grid%athratenlw. ')
 endif
  NULLIFY(grid%athratenlw)
ENDIF
IF ( ASSOCIATED( grid%athratensw ) ) THEN 
  DEALLOCATE(grid%athratensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13061,&
'frame/module_domain.f: Failed to deallocate grid%athratensw. ')
 endif
  NULLIFY(grid%athratensw)
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail1 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13069,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail1. ')
 endif
  NULLIFY(grid%hailcast_dhail1)
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail2 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13077,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail2. ')
 endif
  NULLIFY(grid%hailcast_dhail2)
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail3 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13085,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail3. ')
 endif
  NULLIFY(grid%hailcast_dhail3)
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail4 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13093,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail4. ')
 endif
  NULLIFY(grid%hailcast_dhail4)
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail5 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13101,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail5. ')
 endif
  NULLIFY(grid%hailcast_dhail5)
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_max ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13109,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_max. ')
 endif
  NULLIFY(grid%hailcast_diam_max)
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_mean ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13117,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_mean. ')
 endif
  NULLIFY(grid%hailcast_diam_mean)
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_std ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13125,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_std. ')
 endif
  NULLIFY(grid%hailcast_diam_std)
ENDIF
IF ( ASSOCIATED( grid%hailcast_wup_mask ) ) THEN 
  DEALLOCATE(grid%hailcast_wup_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13133,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_wup_mask. ')
 endif
  NULLIFY(grid%hailcast_wup_mask)
ENDIF
IF ( ASSOCIATED( grid%hailcast_wdur ) ) THEN 
  DEALLOCATE(grid%hailcast_wdur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13141,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_wdur. ')
 endif
  NULLIFY(grid%hailcast_wdur)
ENDIF
IF ( ASSOCIATED( grid%haildtacttime ) ) THEN 
  DEALLOCATE(grid%haildtacttime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13149,&
'frame/module_domain.f: Failed to deallocate grid%haildtacttime. ')
 endif
  NULLIFY(grid%haildtacttime)
ENDIF
IF ( ASSOCIATED( grid%ic_flashcount ) ) THEN 
  DEALLOCATE(grid%ic_flashcount,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13157,&
'frame/module_domain.f: Failed to deallocate grid%ic_flashcount. ')
 endif
  NULLIFY(grid%ic_flashcount)
ENDIF
IF ( ASSOCIATED( grid%ic_flashrate ) ) THEN 
  DEALLOCATE(grid%ic_flashrate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13165,&
'frame/module_domain.f: Failed to deallocate grid%ic_flashrate. ')
 endif
  NULLIFY(grid%ic_flashrate)
ENDIF
IF ( ASSOCIATED( grid%cg_flashcount ) ) THEN 
  DEALLOCATE(grid%cg_flashcount,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13173,&
'frame/module_domain.f: Failed to deallocate grid%cg_flashcount. ')
 endif
  NULLIFY(grid%cg_flashcount)
ENDIF
IF ( ASSOCIATED( grid%cg_flashrate ) ) THEN 
  DEALLOCATE(grid%cg_flashrate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13181,&
'frame/module_domain.f: Failed to deallocate grid%cg_flashrate. ')
 endif
  NULLIFY(grid%cg_flashrate)
ENDIF
IF ( ASSOCIATED( grid%iccg_in_num ) ) THEN 
  DEALLOCATE(grid%iccg_in_num,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13189,&
'frame/module_domain.f: Failed to deallocate grid%iccg_in_num. ')
 endif
  NULLIFY(grid%iccg_in_num)
ENDIF
IF ( ASSOCIATED( grid%iccg_in_den ) ) THEN 
  DEALLOCATE(grid%iccg_in_den,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13197,&
'frame/module_domain.f: Failed to deallocate grid%iccg_in_den. ')
 endif
  NULLIFY(grid%iccg_in_den)
ENDIF
IF ( ASSOCIATED( grid%fdob%varobs ) ) THEN 
  DEALLOCATE(grid%fdob%varobs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13205,&
'frame/module_domain.f: Failed to deallocate grid%fdob%varobs. ')
 endif
  NULLIFY(grid%fdob%varobs)
ENDIF
IF ( ASSOCIATED( grid%fdob%errf ) ) THEN 
  DEALLOCATE(grid%fdob%errf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13213,&
'frame/module_domain.f: Failed to deallocate grid%fdob%errf. ')
 endif
  NULLIFY(grid%fdob%errf)
ENDIF
IF ( ASSOCIATED( grid%fdob%timeob ) ) THEN 
  DEALLOCATE(grid%fdob%timeob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13221,&
'frame/module_domain.f: Failed to deallocate grid%fdob%timeob. ')
 endif
  NULLIFY(grid%fdob%timeob)
ENDIF
IF ( ASSOCIATED( grid%fdob%nlevs_ob ) ) THEN 
  DEALLOCATE(grid%fdob%nlevs_ob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13229,&
'frame/module_domain.f: Failed to deallocate grid%fdob%nlevs_ob. ')
 endif
  NULLIFY(grid%fdob%nlevs_ob)
ENDIF
IF ( ASSOCIATED( grid%fdob%lev_in_ob ) ) THEN 
  DEALLOCATE(grid%fdob%lev_in_ob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13237,&
'frame/module_domain.f: Failed to deallocate grid%fdob%lev_in_ob. ')
 endif
  NULLIFY(grid%fdob%lev_in_ob)
ENDIF
IF ( ASSOCIATED( grid%fdob%plfo ) ) THEN 
  DEALLOCATE(grid%fdob%plfo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13245,&
'frame/module_domain.f: Failed to deallocate grid%fdob%plfo. ')
 endif
  NULLIFY(grid%fdob%plfo)
ENDIF
IF ( ASSOCIATED( grid%fdob%elevob ) ) THEN 
  DEALLOCATE(grid%fdob%elevob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13253,&
'frame/module_domain.f: Failed to deallocate grid%fdob%elevob. ')
 endif
  NULLIFY(grid%fdob%elevob)
ENDIF
IF ( ASSOCIATED( grid%fdob%rio ) ) THEN 
  DEALLOCATE(grid%fdob%rio,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13261,&
'frame/module_domain.f: Failed to deallocate grid%fdob%rio. ')
 endif
  NULLIFY(grid%fdob%rio)
ENDIF
IF ( ASSOCIATED( grid%fdob%rjo ) ) THEN 
  DEALLOCATE(grid%fdob%rjo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13269,&
'frame/module_domain.f: Failed to deallocate grid%fdob%rjo. ')
 endif
  NULLIFY(grid%fdob%rjo)
ENDIF
IF ( ASSOCIATED( grid%fdob%rko ) ) THEN 
  DEALLOCATE(grid%fdob%rko,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13277,&
'frame/module_domain.f: Failed to deallocate grid%fdob%rko. ')
 endif
  NULLIFY(grid%fdob%rko)
ENDIF
IF ( ASSOCIATED( grid%fdob%obsprt ) ) THEN 
  DEALLOCATE(grid%fdob%obsprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13285,&
'frame/module_domain.f: Failed to deallocate grid%fdob%obsprt. ')
 endif
  NULLIFY(grid%fdob%obsprt)
ENDIF
IF ( ASSOCIATED( grid%fdob%latprt ) ) THEN 
  DEALLOCATE(grid%fdob%latprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13293,&
'frame/module_domain.f: Failed to deallocate grid%fdob%latprt. ')
 endif
  NULLIFY(grid%fdob%latprt)
ENDIF
IF ( ASSOCIATED( grid%fdob%lonprt ) ) THEN 
  DEALLOCATE(grid%fdob%lonprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13301,&
'frame/module_domain.f: Failed to deallocate grid%fdob%lonprt. ')
 endif
  NULLIFY(grid%fdob%lonprt)
ENDIF
IF ( ASSOCIATED( grid%fdob%mlatprt ) ) THEN 
  DEALLOCATE(grid%fdob%mlatprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13309,&
'frame/module_domain.f: Failed to deallocate grid%fdob%mlatprt. ')
 endif
  NULLIFY(grid%fdob%mlatprt)
ENDIF
IF ( ASSOCIATED( grid%fdob%mlonprt ) ) THEN 
  DEALLOCATE(grid%fdob%mlonprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13317,&
'frame/module_domain.f: Failed to deallocate grid%fdob%mlonprt. ')
 endif
  NULLIFY(grid%fdob%mlonprt)
ENDIF
IF ( ASSOCIATED( grid%fdob%stnidprt ) ) THEN 
  DEALLOCATE(grid%fdob%stnidprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13325,&
'frame/module_domain.f: Failed to deallocate grid%fdob%stnidprt. ')
 endif
  NULLIFY(grid%fdob%stnidprt)
ENDIF
IF ( ASSOCIATED( grid%fdob%base_state ) ) THEN 
  DEALLOCATE(grid%fdob%base_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13333,&
'frame/module_domain.f: Failed to deallocate grid%fdob%base_state. ')
 endif
  NULLIFY(grid%fdob%base_state)
ENDIF
IF ( ASSOCIATED( grid%t_xxx ) ) THEN 
  DEALLOCATE(grid%t_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13341,&
'frame/module_domain.f: Failed to deallocate grid%t_xxx. ')
 endif
  NULLIFY(grid%t_xxx)
ENDIF
IF ( ASSOCIATED( grid%u_xxx ) ) THEN 
  DEALLOCATE(grid%u_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13349,&
'frame/module_domain.f: Failed to deallocate grid%u_xxx. ')
 endif
  NULLIFY(grid%u_xxx)
ENDIF
IF ( ASSOCIATED( grid%ru_xxx ) ) THEN 
  DEALLOCATE(grid%ru_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13357,&
'frame/module_domain.f: Failed to deallocate grid%ru_xxx. ')
 endif
  NULLIFY(grid%ru_xxx)
ENDIF
IF ( ASSOCIATED( grid%v_xxx ) ) THEN 
  DEALLOCATE(grid%v_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13365,&
'frame/module_domain.f: Failed to deallocate grid%v_xxx. ')
 endif
  NULLIFY(grid%v_xxx)
ENDIF
IF ( ASSOCIATED( grid%rv_xxx ) ) THEN 
  DEALLOCATE(grid%rv_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13373,&
'frame/module_domain.f: Failed to deallocate grid%rv_xxx. ')
 endif
  NULLIFY(grid%rv_xxx)
ENDIF
IF ( ASSOCIATED( grid%w_xxx ) ) THEN 
  DEALLOCATE(grid%w_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13381,&
'frame/module_domain.f: Failed to deallocate grid%w_xxx. ')
 endif
  NULLIFY(grid%w_xxx)
ENDIF
IF ( ASSOCIATED( grid%ww_xxx ) ) THEN 
  DEALLOCATE(grid%ww_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13389,&
'frame/module_domain.f: Failed to deallocate grid%ww_xxx. ')
 endif
  NULLIFY(grid%ww_xxx)
ENDIF
IF ( ASSOCIATED( grid%ph_xxx ) ) THEN 
  DEALLOCATE(grid%ph_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13397,&
'frame/module_domain.f: Failed to deallocate grid%ph_xxx. ')
 endif
  NULLIFY(grid%ph_xxx)
ENDIF
IF ( ASSOCIATED( grid%dum_yyy ) ) THEN 
  DEALLOCATE(grid%dum_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13405,&
'frame/module_domain.f: Failed to deallocate grid%dum_yyy. ')
 endif
  NULLIFY(grid%dum_yyy)
ENDIF
IF ( ASSOCIATED( grid%fourd_xxx ) ) THEN 
  DEALLOCATE(grid%fourd_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13413,&
'frame/module_domain.f: Failed to deallocate grid%fourd_xxx. ')
 endif
  NULLIFY(grid%fourd_xxx)
ENDIF
IF ( ASSOCIATED( grid%clat_xxx ) ) THEN 
  DEALLOCATE(grid%clat_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13421,&
'frame/module_domain.f: Failed to deallocate grid%clat_xxx. ')
 endif
  NULLIFY(grid%clat_xxx)
ENDIF
IF ( ASSOCIATED( grid%ht_xxx ) ) THEN 
  DEALLOCATE(grid%ht_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13429,&
'frame/module_domain.f: Failed to deallocate grid%ht_xxx. ')
 endif
  NULLIFY(grid%ht_xxx)
ENDIF
IF ( ASSOCIATED( grid%mf_xxx ) ) THEN 
  DEALLOCATE(grid%mf_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13437,&
'frame/module_domain.f: Failed to deallocate grid%mf_xxx. ')
 endif
  NULLIFY(grid%mf_xxx)
ENDIF
IF ( ASSOCIATED( grid%dif_analysis ) ) THEN 
  DEALLOCATE(grid%dif_analysis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13445,&
'frame/module_domain.f: Failed to deallocate grid%dif_analysis. ')
 endif
  NULLIFY(grid%dif_analysis)
ENDIF
IF ( ASSOCIATED( grid%dif_xxx ) ) THEN 
  DEALLOCATE(grid%dif_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13453,&
'frame/module_domain.f: Failed to deallocate grid%dif_xxx. ')
 endif
  NULLIFY(grid%dif_xxx)
ENDIF
IF ( ASSOCIATED( grid%dif_yyy ) ) THEN 
  DEALLOCATE(grid%dif_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13461,&
'frame/module_domain.f: Failed to deallocate grid%dif_yyy. ')
 endif
  NULLIFY(grid%dif_yyy)
ENDIF
IF ( ASSOCIATED( grid%lfn_hist ) ) THEN 
  DEALLOCATE(grid%lfn_hist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13469,&
'frame/module_domain.f: Failed to deallocate grid%lfn_hist. ')
 endif
  NULLIFY(grid%lfn_hist)
ENDIF
IF ( ASSOCIATED( grid%lfn_time ) ) THEN 
  DEALLOCATE(grid%lfn_time,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13477,&
'frame/module_domain.f: Failed to deallocate grid%lfn_time. ')
 endif
  NULLIFY(grid%lfn_time)
ENDIF
IF ( ASSOCIATED( grid%nfuel_cat ) ) THEN 
  DEALLOCATE(grid%nfuel_cat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13485,&
'frame/module_domain.f: Failed to deallocate grid%nfuel_cat. ')
 endif
  NULLIFY(grid%nfuel_cat)
ENDIF
IF ( ASSOCIATED( grid%zsf ) ) THEN 
  DEALLOCATE(grid%zsf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13493,&
'frame/module_domain.f: Failed to deallocate grid%zsf. ')
 endif
  NULLIFY(grid%zsf)
ENDIF
IF ( ASSOCIATED( grid%dzdxf ) ) THEN 
  DEALLOCATE(grid%dzdxf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13501,&
'frame/module_domain.f: Failed to deallocate grid%dzdxf. ')
 endif
  NULLIFY(grid%dzdxf)
ENDIF
IF ( ASSOCIATED( grid%dzdyf ) ) THEN 
  DEALLOCATE(grid%dzdyf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13509,&
'frame/module_domain.f: Failed to deallocate grid%dzdyf. ')
 endif
  NULLIFY(grid%dzdyf)
ENDIF
IF ( ASSOCIATED( grid%tign_g ) ) THEN 
  DEALLOCATE(grid%tign_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13517,&
'frame/module_domain.f: Failed to deallocate grid%tign_g. ')
 endif
  NULLIFY(grid%tign_g)
ENDIF
IF ( ASSOCIATED( grid%rthfrten ) ) THEN 
  DEALLOCATE(grid%rthfrten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13525,&
'frame/module_domain.f: Failed to deallocate grid%rthfrten. ')
 endif
  NULLIFY(grid%rthfrten)
ENDIF
IF ( ASSOCIATED( grid%rqvfrten ) ) THEN 
  DEALLOCATE(grid%rqvfrten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13533,&
'frame/module_domain.f: Failed to deallocate grid%rqvfrten. ')
 endif
  NULLIFY(grid%rqvfrten)
ENDIF
IF ( ASSOCIATED( grid%avg_fuel_frac ) ) THEN 
  DEALLOCATE(grid%avg_fuel_frac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13541,&
'frame/module_domain.f: Failed to deallocate grid%avg_fuel_frac. ')
 endif
  NULLIFY(grid%avg_fuel_frac)
ENDIF
IF ( ASSOCIATED( grid%grnhfx ) ) THEN 
  DEALLOCATE(grid%grnhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13549,&
'frame/module_domain.f: Failed to deallocate grid%grnhfx. ')
 endif
  NULLIFY(grid%grnhfx)
ENDIF
IF ( ASSOCIATED( grid%grnqfx ) ) THEN 
  DEALLOCATE(grid%grnqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13557,&
'frame/module_domain.f: Failed to deallocate grid%grnqfx. ')
 endif
  NULLIFY(grid%grnqfx)
ENDIF
IF ( ASSOCIATED( grid%canhfx ) ) THEN 
  DEALLOCATE(grid%canhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13565,&
'frame/module_domain.f: Failed to deallocate grid%canhfx. ')
 endif
  NULLIFY(grid%canhfx)
ENDIF
IF ( ASSOCIATED( grid%canqfx ) ) THEN 
  DEALLOCATE(grid%canqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13573,&
'frame/module_domain.f: Failed to deallocate grid%canqfx. ')
 endif
  NULLIFY(grid%canqfx)
ENDIF
IF ( ASSOCIATED( grid%uah ) ) THEN 
  DEALLOCATE(grid%uah,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13581,&
'frame/module_domain.f: Failed to deallocate grid%uah. ')
 endif
  NULLIFY(grid%uah)
ENDIF
IF ( ASSOCIATED( grid%vah ) ) THEN 
  DEALLOCATE(grid%vah,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13589,&
'frame/module_domain.f: Failed to deallocate grid%vah. ')
 endif
  NULLIFY(grid%vah)
ENDIF
IF ( ASSOCIATED( grid%grnhfx_fu ) ) THEN 
  DEALLOCATE(grid%grnhfx_fu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13597,&
'frame/module_domain.f: Failed to deallocate grid%grnhfx_fu. ')
 endif
  NULLIFY(grid%grnhfx_fu)
ENDIF
IF ( ASSOCIATED( grid%grnqfx_fu ) ) THEN 
  DEALLOCATE(grid%grnqfx_fu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13605,&
'frame/module_domain.f: Failed to deallocate grid%grnqfx_fu. ')
 endif
  NULLIFY(grid%grnqfx_fu)
ENDIF
IF ( ASSOCIATED( grid%lfn ) ) THEN 
  DEALLOCATE(grid%lfn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13613,&
'frame/module_domain.f: Failed to deallocate grid%lfn. ')
 endif
  NULLIFY(grid%lfn)
ENDIF
IF ( ASSOCIATED( grid%lfn_0 ) ) THEN 
  DEALLOCATE(grid%lfn_0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13621,&
'frame/module_domain.f: Failed to deallocate grid%lfn_0. ')
 endif
  NULLIFY(grid%lfn_0)
ENDIF
IF ( ASSOCIATED( grid%lfn_1 ) ) THEN 
  DEALLOCATE(grid%lfn_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13629,&
'frame/module_domain.f: Failed to deallocate grid%lfn_1. ')
 endif
  NULLIFY(grid%lfn_1)
ENDIF
IF ( ASSOCIATED( grid%lfn_2 ) ) THEN 
  DEALLOCATE(grid%lfn_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13637,&
'frame/module_domain.f: Failed to deallocate grid%lfn_2. ')
 endif
  NULLIFY(grid%lfn_2)
ENDIF
IF ( ASSOCIATED( grid%lfn_s0 ) ) THEN 
  DEALLOCATE(grid%lfn_s0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13645,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s0. ')
 endif
  NULLIFY(grid%lfn_s0)
ENDIF
IF ( ASSOCIATED( grid%lfn_s1 ) ) THEN 
  DEALLOCATE(grid%lfn_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13653,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s1. ')
 endif
  NULLIFY(grid%lfn_s1)
ENDIF
IF ( ASSOCIATED( grid%lfn_s2 ) ) THEN 
  DEALLOCATE(grid%lfn_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13661,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s2. ')
 endif
  NULLIFY(grid%lfn_s2)
ENDIF
IF ( ASSOCIATED( grid%lfn_s3 ) ) THEN 
  DEALLOCATE(grid%lfn_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13669,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s3. ')
 endif
  NULLIFY(grid%lfn_s3)
ENDIF
IF ( ASSOCIATED( grid%fuel_frac ) ) THEN 
  DEALLOCATE(grid%fuel_frac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13677,&
'frame/module_domain.f: Failed to deallocate grid%fuel_frac. ')
 endif
  NULLIFY(grid%fuel_frac)
ENDIF
IF ( ASSOCIATED( grid%fire_area ) ) THEN 
  DEALLOCATE(grid%fire_area,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13685,&
'frame/module_domain.f: Failed to deallocate grid%fire_area. ')
 endif
  NULLIFY(grid%fire_area)
ENDIF
IF ( ASSOCIATED( grid%uf ) ) THEN 
  DEALLOCATE(grid%uf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13693,&
'frame/module_domain.f: Failed to deallocate grid%uf. ')
 endif
  NULLIFY(grid%uf)
ENDIF
IF ( ASSOCIATED( grid%vf ) ) THEN 
  DEALLOCATE(grid%vf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13701,&
'frame/module_domain.f: Failed to deallocate grid%vf. ')
 endif
  NULLIFY(grid%vf)
ENDIF
IF ( ASSOCIATED( grid%fgrnhfx ) ) THEN 
  DEALLOCATE(grid%fgrnhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13709,&
'frame/module_domain.f: Failed to deallocate grid%fgrnhfx. ')
 endif
  NULLIFY(grid%fgrnhfx)
ENDIF
IF ( ASSOCIATED( grid%fgrnqfx ) ) THEN 
  DEALLOCATE(grid%fgrnqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13717,&
'frame/module_domain.f: Failed to deallocate grid%fgrnqfx. ')
 endif
  NULLIFY(grid%fgrnqfx)
ENDIF
IF ( ASSOCIATED( grid%fcanhfx ) ) THEN 
  DEALLOCATE(grid%fcanhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13725,&
'frame/module_domain.f: Failed to deallocate grid%fcanhfx. ')
 endif
  NULLIFY(grid%fcanhfx)
ENDIF
IF ( ASSOCIATED( grid%fcanqfx ) ) THEN 
  DEALLOCATE(grid%fcanqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13733,&
'frame/module_domain.f: Failed to deallocate grid%fcanqfx. ')
 endif
  NULLIFY(grid%fcanqfx)
ENDIF
IF ( ASSOCIATED( grid%ros ) ) THEN 
  DEALLOCATE(grid%ros,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13741,&
'frame/module_domain.f: Failed to deallocate grid%ros. ')
 endif
  NULLIFY(grid%ros)
ENDIF
IF ( ASSOCIATED( grid%burnt_area_dt ) ) THEN 
  DEALLOCATE(grid%burnt_area_dt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13749,&
'frame/module_domain.f: Failed to deallocate grid%burnt_area_dt. ')
 endif
  NULLIFY(grid%burnt_area_dt)
ENDIF
IF ( ASSOCIATED( grid%flame_length ) ) THEN 
  DEALLOCATE(grid%flame_length,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13757,&
'frame/module_domain.f: Failed to deallocate grid%flame_length. ')
 endif
  NULLIFY(grid%flame_length)
ENDIF
IF ( ASSOCIATED( grid%ros_front ) ) THEN 
  DEALLOCATE(grid%ros_front,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13765,&
'frame/module_domain.f: Failed to deallocate grid%ros_front. ')
 endif
  NULLIFY(grid%ros_front)
ENDIF
IF ( ASSOCIATED( grid%fmc_g ) ) THEN 
  DEALLOCATE(grid%fmc_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13773,&
'frame/module_domain.f: Failed to deallocate grid%fmc_g. ')
 endif
  NULLIFY(grid%fmc_g)
ENDIF
IF ( ASSOCIATED( grid%fmc_gc ) ) THEN 
  DEALLOCATE(grid%fmc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13781,&
'frame/module_domain.f: Failed to deallocate grid%fmc_gc. ')
 endif
  NULLIFY(grid%fmc_gc)
ENDIF
IF ( ASSOCIATED( grid%fmep ) ) THEN 
  DEALLOCATE(grid%fmep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13789,&
'frame/module_domain.f: Failed to deallocate grid%fmep. ')
 endif
  NULLIFY(grid%fmep)
ENDIF
IF ( ASSOCIATED( grid%fmc_equi ) ) THEN 
  DEALLOCATE(grid%fmc_equi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13797,&
'frame/module_domain.f: Failed to deallocate grid%fmc_equi. ')
 endif
  NULLIFY(grid%fmc_equi)
ENDIF
IF ( ASSOCIATED( grid%fmc_lag ) ) THEN 
  DEALLOCATE(grid%fmc_lag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13805,&
'frame/module_domain.f: Failed to deallocate grid%fmc_lag. ')
 endif
  NULLIFY(grid%fmc_lag)
ENDIF
IF ( ASSOCIATED( grid%rain_old ) ) THEN 
  DEALLOCATE(grid%rain_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13813,&
'frame/module_domain.f: Failed to deallocate grid%rain_old. ')
 endif
  NULLIFY(grid%rain_old)
ENDIF
IF ( ASSOCIATED( grid%t2_old ) ) THEN 
  DEALLOCATE(grid%t2_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13821,&
'frame/module_domain.f: Failed to deallocate grid%t2_old. ')
 endif
  NULLIFY(grid%t2_old)
ENDIF
IF ( ASSOCIATED( grid%q2_old ) ) THEN 
  DEALLOCATE(grid%q2_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13829,&
'frame/module_domain.f: Failed to deallocate grid%q2_old. ')
 endif
  NULLIFY(grid%q2_old)
ENDIF
IF ( ASSOCIATED( grid%psfc_old ) ) THEN 
  DEALLOCATE(grid%psfc_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13837,&
'frame/module_domain.f: Failed to deallocate grid%psfc_old. ')
 endif
  NULLIFY(grid%psfc_old)
ENDIF
IF ( ASSOCIATED( grid%rh_fire ) ) THEN 
  DEALLOCATE(grid%rh_fire,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13845,&
'frame/module_domain.f: Failed to deallocate grid%rh_fire. ')
 endif
  NULLIFY(grid%rh_fire)
ENDIF
IF ( ASSOCIATED( grid%fxlong ) ) THEN 
  DEALLOCATE(grid%fxlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13853,&
'frame/module_domain.f: Failed to deallocate grid%fxlong. ')
 endif
  NULLIFY(grid%fxlong)
ENDIF
IF ( ASSOCIATED( grid%fxlat ) ) THEN 
  DEALLOCATE(grid%fxlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13861,&
'frame/module_domain.f: Failed to deallocate grid%fxlat. ')
 endif
  NULLIFY(grid%fxlat)
ENDIF
IF ( ASSOCIATED( grid%fuel_time ) ) THEN 
  DEALLOCATE(grid%fuel_time,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13869,&
'frame/module_domain.f: Failed to deallocate grid%fuel_time. ')
 endif
  NULLIFY(grid%fuel_time)
ENDIF
IF ( ASSOCIATED( grid%bbb ) ) THEN 
  DEALLOCATE(grid%bbb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13877,&
'frame/module_domain.f: Failed to deallocate grid%bbb. ')
 endif
  NULLIFY(grid%bbb)
ENDIF
IF ( ASSOCIATED( grid%betafl ) ) THEN 
  DEALLOCATE(grid%betafl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13885,&
'frame/module_domain.f: Failed to deallocate grid%betafl. ')
 endif
  NULLIFY(grid%betafl)
ENDIF
IF ( ASSOCIATED( grid%phiwc ) ) THEN 
  DEALLOCATE(grid%phiwc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13893,&
'frame/module_domain.f: Failed to deallocate grid%phiwc. ')
 endif
  NULLIFY(grid%phiwc)
ENDIF
IF ( ASSOCIATED( grid%r_0 ) ) THEN 
  DEALLOCATE(grid%r_0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13901,&
'frame/module_domain.f: Failed to deallocate grid%r_0. ')
 endif
  NULLIFY(grid%r_0)
ENDIF
IF ( ASSOCIATED( grid%fgip ) ) THEN 
  DEALLOCATE(grid%fgip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13909,&
'frame/module_domain.f: Failed to deallocate grid%fgip. ')
 endif
  NULLIFY(grid%fgip)
ENDIF
IF ( ASSOCIATED( grid%ischap ) ) THEN 
  DEALLOCATE(grid%ischap,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13917,&
'frame/module_domain.f: Failed to deallocate grid%ischap. ')
 endif
  NULLIFY(grid%ischap)
ENDIF
IF ( ASSOCIATED( grid%fz0 ) ) THEN 
  DEALLOCATE(grid%fz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13925,&
'frame/module_domain.f: Failed to deallocate grid%fz0. ')
 endif
  NULLIFY(grid%fz0)
ENDIF
IF ( ASSOCIATED( grid%iboros ) ) THEN 
  DEALLOCATE(grid%iboros,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13933,&
'frame/module_domain.f: Failed to deallocate grid%iboros. ')
 endif
  NULLIFY(grid%iboros)
ENDIF
IF ( ASSOCIATED( grid%tracer ) ) THEN 
  DEALLOCATE(grid%tracer,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13941,&
'frame/module_domain.f: Failed to deallocate grid%tracer. ')
 endif
  NULLIFY(grid%tracer)
ENDIF
IF ( ASSOCIATED( grid%tracer_bxs ) ) THEN 
  DEALLOCATE(grid%tracer_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13949,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bxs. ')
 endif
  NULLIFY(grid%tracer_bxs)
ENDIF
IF ( ASSOCIATED( grid%tracer_bxe ) ) THEN 
  DEALLOCATE(grid%tracer_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13957,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bxe. ')
 endif
  NULLIFY(grid%tracer_bxe)
ENDIF
IF ( ASSOCIATED( grid%tracer_bys ) ) THEN 
  DEALLOCATE(grid%tracer_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13965,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bys. ')
 endif
  NULLIFY(grid%tracer_bys)
ENDIF
IF ( ASSOCIATED( grid%tracer_bye ) ) THEN 
  DEALLOCATE(grid%tracer_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13973,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bye. ')
 endif
  NULLIFY(grid%tracer_bye)
ENDIF
IF ( ASSOCIATED( grid%tracer_btxs ) ) THEN 
  DEALLOCATE(grid%tracer_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13981,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btxs. ')
 endif
  NULLIFY(grid%tracer_btxs)
ENDIF
IF ( ASSOCIATED( grid%tracer_btxe ) ) THEN 
  DEALLOCATE(grid%tracer_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13989,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btxe. ')
 endif
  NULLIFY(grid%tracer_btxe)
ENDIF
IF ( ASSOCIATED( grid%tracer_btys ) ) THEN 
  DEALLOCATE(grid%tracer_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13997,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btys. ')
 endif
  NULLIFY(grid%tracer_btys)
ENDIF
IF ( ASSOCIATED( grid%tracer_btye ) ) THEN 
  DEALLOCATE(grid%tracer_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14005,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btye. ')
 endif
  NULLIFY(grid%tracer_btye)
ENDIF
IF ( ASSOCIATED( grid%fs_fire_rosdt ) ) THEN 
  DEALLOCATE(grid%fs_fire_rosdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14013,&
'frame/module_domain.f: Failed to deallocate grid%fs_fire_rosdt. ')
 endif
  NULLIFY(grid%fs_fire_rosdt)
ENDIF
IF ( ASSOCIATED( grid%fs_fire_area ) ) THEN 
  DEALLOCATE(grid%fs_fire_area,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14021,&
'frame/module_domain.f: Failed to deallocate grid%fs_fire_area. ')
 endif
  NULLIFY(grid%fs_fire_area)
ENDIF
IF ( ASSOCIATED( grid%fs_fuel_spotting_risk ) ) THEN 
  DEALLOCATE(grid%fs_fuel_spotting_risk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14029,&
'frame/module_domain.f: Failed to deallocate grid%fs_fuel_spotting_risk. ')
 endif
  NULLIFY(grid%fs_fuel_spotting_risk)
ENDIF
IF ( ASSOCIATED( grid%fs_count_landed_all ) ) THEN 
  DEALLOCATE(grid%fs_count_landed_all,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14037,&
'frame/module_domain.f: Failed to deallocate grid%fs_count_landed_all. ')
 endif
  NULLIFY(grid%fs_count_landed_all)
ENDIF
IF ( ASSOCIATED( grid%fs_count_landed_hist ) ) THEN 
  DEALLOCATE(grid%fs_count_landed_hist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14045,&
'frame/module_domain.f: Failed to deallocate grid%fs_count_landed_hist. ')
 endif
  NULLIFY(grid%fs_count_landed_hist)
ENDIF
IF ( ASSOCIATED( grid%fs_landing_mask ) ) THEN 
  DEALLOCATE(grid%fs_landing_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14053,&
'frame/module_domain.f: Failed to deallocate grid%fs_landing_mask. ')
 endif
  NULLIFY(grid%fs_landing_mask)
ENDIF
IF ( ASSOCIATED( grid%fs_gen_inst ) ) THEN 
  DEALLOCATE(grid%fs_gen_inst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14061,&
'frame/module_domain.f: Failed to deallocate grid%fs_gen_inst. ')
 endif
  NULLIFY(grid%fs_gen_inst)
ENDIF
IF ( ASSOCIATED( grid%fs_frac_landed ) ) THEN 
  DEALLOCATE(grid%fs_frac_landed,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14069,&
'frame/module_domain.f: Failed to deallocate grid%fs_frac_landed. ')
 endif
  NULLIFY(grid%fs_frac_landed)
ENDIF
IF ( ASSOCIATED( grid%fs_spotting_lkhd ) ) THEN 
  DEALLOCATE(grid%fs_spotting_lkhd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14077,&
'frame/module_domain.f: Failed to deallocate grid%fs_spotting_lkhd. ')
 endif
  NULLIFY(grid%fs_spotting_lkhd)
ENDIF
IF ( ASSOCIATED( grid%fs_p_id ) ) THEN 
  DEALLOCATE(grid%fs_p_id,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14085,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_id. ')
 endif
  NULLIFY(grid%fs_p_id)
ENDIF
IF ( ASSOCIATED( grid%fs_p_src ) ) THEN 
  DEALLOCATE(grid%fs_p_src,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14093,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_src. ')
 endif
  NULLIFY(grid%fs_p_src)
ENDIF
IF ( ASSOCIATED( grid%fs_p_dt ) ) THEN 
  DEALLOCATE(grid%fs_p_dt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14101,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_dt. ')
 endif
  NULLIFY(grid%fs_p_dt)
ENDIF
IF ( ASSOCIATED( grid%fs_p_x ) ) THEN 
  DEALLOCATE(grid%fs_p_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14109,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_x. ')
 endif
  NULLIFY(grid%fs_p_x)
ENDIF
IF ( ASSOCIATED( grid%fs_p_y ) ) THEN 
  DEALLOCATE(grid%fs_p_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14117,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_y. ')
 endif
  NULLIFY(grid%fs_p_y)
ENDIF
IF ( ASSOCIATED( grid%fs_p_z ) ) THEN 
  DEALLOCATE(grid%fs_p_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14125,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_z. ')
 endif
  NULLIFY(grid%fs_p_z)
ENDIF
IF ( ASSOCIATED( grid%fs_p_mass ) ) THEN 
  DEALLOCATE(grid%fs_p_mass,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14133,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_mass. ')
 endif
  NULLIFY(grid%fs_p_mass)
ENDIF
IF ( ASSOCIATED( grid%fs_p_diam ) ) THEN 
  DEALLOCATE(grid%fs_p_diam,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14141,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_diam. ')
 endif
  NULLIFY(grid%fs_p_diam)
ENDIF
IF ( ASSOCIATED( grid%fs_p_effd ) ) THEN 
  DEALLOCATE(grid%fs_p_effd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14149,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_effd. ')
 endif
  NULLIFY(grid%fs_p_effd)
ENDIF
IF ( ASSOCIATED( grid%fs_p_temp ) ) THEN 
  DEALLOCATE(grid%fs_p_temp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14157,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_temp. ')
 endif
  NULLIFY(grid%fs_p_temp)
ENDIF
IF ( ASSOCIATED( grid%fs_p_tvel ) ) THEN 
  DEALLOCATE(grid%fs_p_tvel,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14165,&
'frame/module_domain.f: Failed to deallocate grid%fs_p_tvel. ')
 endif
  NULLIFY(grid%fs_p_tvel)
ENDIF
IF ( ASSOCIATED( grid%avgflx_rum ) ) THEN 
  DEALLOCATE(grid%avgflx_rum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14173,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_rum. ')
 endif
  NULLIFY(grid%avgflx_rum)
ENDIF
IF ( ASSOCIATED( grid%avgflx_rvm ) ) THEN 
  DEALLOCATE(grid%avgflx_rvm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14181,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_rvm. ')
 endif
  NULLIFY(grid%avgflx_rvm)
ENDIF
IF ( ASSOCIATED( grid%avgflx_wwm ) ) THEN 
  DEALLOCATE(grid%avgflx_wwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14189,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_wwm. ')
 endif
  NULLIFY(grid%avgflx_wwm)
ENDIF
IF ( ASSOCIATED( grid%avgflx_cfu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_cfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14197,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_cfu1. ')
 endif
  NULLIFY(grid%avgflx_cfu1)
ENDIF
IF ( ASSOCIATED( grid%avgflx_cfd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_cfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14205,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_cfd1. ')
 endif
  NULLIFY(grid%avgflx_cfd1)
ENDIF
IF ( ASSOCIATED( grid%avgflx_dfu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_dfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14213,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_dfu1. ')
 endif
  NULLIFY(grid%avgflx_dfu1)
ENDIF
IF ( ASSOCIATED( grid%avgflx_efu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_efu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14221,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_efu1. ')
 endif
  NULLIFY(grid%avgflx_efu1)
ENDIF
IF ( ASSOCIATED( grid%avgflx_dfd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_dfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14229,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_dfd1. ')
 endif
  NULLIFY(grid%avgflx_dfd1)
ENDIF
IF ( ASSOCIATED( grid%avgflx_efd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_efd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14237,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_efd1. ')
 endif
  NULLIFY(grid%avgflx_efd1)
ENDIF
IF ( ASSOCIATED( grid%cfu1 ) ) THEN 
  DEALLOCATE(grid%cfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14245,&
'frame/module_domain.f: Failed to deallocate grid%cfu1. ')
 endif
  NULLIFY(grid%cfu1)
ENDIF
IF ( ASSOCIATED( grid%cfd1 ) ) THEN 
  DEALLOCATE(grid%cfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14253,&
'frame/module_domain.f: Failed to deallocate grid%cfd1. ')
 endif
  NULLIFY(grid%cfd1)
ENDIF
IF ( ASSOCIATED( grid%dfu1 ) ) THEN 
  DEALLOCATE(grid%dfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14261,&
'frame/module_domain.f: Failed to deallocate grid%dfu1. ')
 endif
  NULLIFY(grid%dfu1)
ENDIF
IF ( ASSOCIATED( grid%efu1 ) ) THEN 
  DEALLOCATE(grid%efu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14269,&
'frame/module_domain.f: Failed to deallocate grid%efu1. ')
 endif
  NULLIFY(grid%efu1)
ENDIF
IF ( ASSOCIATED( grid%dfd1 ) ) THEN 
  DEALLOCATE(grid%dfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14277,&
'frame/module_domain.f: Failed to deallocate grid%dfd1. ')
 endif
  NULLIFY(grid%dfd1)
ENDIF
IF ( ASSOCIATED( grid%efd1 ) ) THEN 
  DEALLOCATE(grid%efd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14285,&
'frame/module_domain.f: Failed to deallocate grid%efd1. ')
 endif
  NULLIFY(grid%efd1)
ENDIF
IF ( ASSOCIATED( grid%vertstrucc ) ) THEN 
  DEALLOCATE(grid%vertstrucc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14293,&
'frame/module_domain.f: Failed to deallocate grid%vertstrucc. ')
 endif
  NULLIFY(grid%vertstrucc)
ENDIF
IF ( ASSOCIATED( grid%vertstrucs ) ) THEN 
  DEALLOCATE(grid%vertstrucs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14301,&
'frame/module_domain.f: Failed to deallocate grid%vertstrucs. ')
 endif
  NULLIFY(grid%vertstrucs)
ENDIF
IF ( ASSOCIATED( grid%field_sf ) ) THEN 
  DEALLOCATE(grid%field_sf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14309,&
'frame/module_domain.f: Failed to deallocate grid%field_sf. ')
 endif
  NULLIFY(grid%field_sf)
ENDIF
IF ( ASSOCIATED( grid%field_pbl ) ) THEN 
  DEALLOCATE(grid%field_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14317,&
'frame/module_domain.f: Failed to deallocate grid%field_pbl. ')
 endif
  NULLIFY(grid%field_pbl)
ENDIF
IF ( ASSOCIATED( grid%field_conv ) ) THEN 
  DEALLOCATE(grid%field_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14325,&
'frame/module_domain.f: Failed to deallocate grid%field_conv. ')
 endif
  NULLIFY(grid%field_conv)
ENDIF
IF ( ASSOCIATED( grid%ru_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%ru_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14333,&
'frame/module_domain.f: Failed to deallocate grid%ru_tendf_stoch. ')
 endif
  NULLIFY(grid%ru_tendf_stoch)
ENDIF
IF ( ASSOCIATED( grid%rv_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%rv_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14341,&
'frame/module_domain.f: Failed to deallocate grid%rv_tendf_stoch. ')
 endif
  NULLIFY(grid%rv_tendf_stoch)
ENDIF
IF ( ASSOCIATED( grid%rt_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%rt_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14349,&
'frame/module_domain.f: Failed to deallocate grid%rt_tendf_stoch. ')
 endif
  NULLIFY(grid%rt_tendf_stoch)
ENDIF
IF ( ASSOCIATED( grid%rand_pert ) ) THEN 
  DEALLOCATE(grid%rand_pert,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14357,&
'frame/module_domain.f: Failed to deallocate grid%rand_pert. ')
 endif
  NULLIFY(grid%rand_pert)
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_conv ) ) THEN 
  DEALLOCATE(grid%pattern_spp_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14365,&
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_conv. ')
 endif
  NULLIFY(grid%pattern_spp_conv)
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_pbl ) ) THEN 
  DEALLOCATE(grid%pattern_spp_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14373,&
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_pbl. ')
 endif
  NULLIFY(grid%pattern_spp_pbl)
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_lsm ) ) THEN 
  DEALLOCATE(grid%pattern_spp_lsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14381,&
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_lsm. ')
 endif
  NULLIFY(grid%pattern_spp_lsm)
ENDIF
IF ( ASSOCIATED( grid%rstoch ) ) THEN 
  DEALLOCATE(grid%rstoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14389,&
'frame/module_domain.f: Failed to deallocate grid%rstoch. ')
 endif
  NULLIFY(grid%rstoch)
ENDIF
IF ( ASSOCIATED( grid%rand_real ) ) THEN 
  DEALLOCATE(grid%rand_real,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14397,&
'frame/module_domain.f: Failed to deallocate grid%rand_real. ')
 endif
  NULLIFY(grid%rand_real)
ENDIF
IF ( ASSOCIATED( grid%rand_imag ) ) THEN 
  DEALLOCATE(grid%rand_imag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14405,&
'frame/module_domain.f: Failed to deallocate grid%rand_imag. ')
 endif
  NULLIFY(grid%rand_imag)
ENDIF
IF ( ASSOCIATED( grid%spstreamforcc ) ) THEN 
  DEALLOCATE(grid%spstreamforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14413,&
'frame/module_domain.f: Failed to deallocate grid%spstreamforcc. ')
 endif
  NULLIFY(grid%spstreamforcc)
ENDIF
IF ( ASSOCIATED( grid%spstreamforcs ) ) THEN 
  DEALLOCATE(grid%spstreamforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14421,&
'frame/module_domain.f: Failed to deallocate grid%spstreamforcs. ')
 endif
  NULLIFY(grid%spstreamforcs)
ENDIF
IF ( ASSOCIATED( grid%spstream_amp ) ) THEN 
  DEALLOCATE(grid%spstream_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14429,&
'frame/module_domain.f: Failed to deallocate grid%spstream_amp. ')
 endif
  NULLIFY(grid%spstream_amp)
ENDIF
IF ( ASSOCIATED( grid%sptforcc ) ) THEN 
  DEALLOCATE(grid%sptforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14437,&
'frame/module_domain.f: Failed to deallocate grid%sptforcc. ')
 endif
  NULLIFY(grid%sptforcc)
ENDIF
IF ( ASSOCIATED( grid%sptforcs ) ) THEN 
  DEALLOCATE(grid%sptforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14445,&
'frame/module_domain.f: Failed to deallocate grid%sptforcs. ')
 endif
  NULLIFY(grid%sptforcs)
ENDIF
IF ( ASSOCIATED( grid%spt_amp ) ) THEN 
  DEALLOCATE(grid%spt_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14453,&
'frame/module_domain.f: Failed to deallocate grid%spt_amp. ')
 endif
  NULLIFY(grid%spt_amp)
ENDIF
IF ( ASSOCIATED( grid%spforcc ) ) THEN 
  DEALLOCATE(grid%spforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14461,&
'frame/module_domain.f: Failed to deallocate grid%spforcc. ')
 endif
  NULLIFY(grid%spforcc)
ENDIF
IF ( ASSOCIATED( grid%spforcs ) ) THEN 
  DEALLOCATE(grid%spforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14469,&
'frame/module_domain.f: Failed to deallocate grid%spforcs. ')
 endif
  NULLIFY(grid%spforcs)
ENDIF
IF ( ASSOCIATED( grid%sp_amp ) ) THEN 
  DEALLOCATE(grid%sp_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14477,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp. ')
 endif
  NULLIFY(grid%sp_amp)
ENDIF
IF ( ASSOCIATED( grid%spforcc2 ) ) THEN 
  DEALLOCATE(grid%spforcc2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14485,&
'frame/module_domain.f: Failed to deallocate grid%spforcc2. ')
 endif
  NULLIFY(grid%spforcc2)
ENDIF
IF ( ASSOCIATED( grid%spforcs2 ) ) THEN 
  DEALLOCATE(grid%spforcs2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14493,&
'frame/module_domain.f: Failed to deallocate grid%spforcs2. ')
 endif
  NULLIFY(grid%spforcs2)
ENDIF
IF ( ASSOCIATED( grid%sp_amp2 ) ) THEN 
  DEALLOCATE(grid%sp_amp2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14501,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp2. ')
 endif
  NULLIFY(grid%sp_amp2)
ENDIF
IF ( ASSOCIATED( grid%spforcc3 ) ) THEN 
  DEALLOCATE(grid%spforcc3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14509,&
'frame/module_domain.f: Failed to deallocate grid%spforcc3. ')
 endif
  NULLIFY(grid%spforcc3)
ENDIF
IF ( ASSOCIATED( grid%spforcs3 ) ) THEN 
  DEALLOCATE(grid%spforcs3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14517,&
'frame/module_domain.f: Failed to deallocate grid%spforcs3. ')
 endif
  NULLIFY(grid%spforcs3)
ENDIF
IF ( ASSOCIATED( grid%sp_amp3 ) ) THEN 
  DEALLOCATE(grid%sp_amp3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14525,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp3. ')
 endif
  NULLIFY(grid%sp_amp3)
ENDIF
IF ( ASSOCIATED( grid%spforcc4 ) ) THEN 
  DEALLOCATE(grid%spforcc4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14533,&
'frame/module_domain.f: Failed to deallocate grid%spforcc4. ')
 endif
  NULLIFY(grid%spforcc4)
ENDIF
IF ( ASSOCIATED( grid%spforcs4 ) ) THEN 
  DEALLOCATE(grid%spforcs4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14541,&
'frame/module_domain.f: Failed to deallocate grid%spforcs4. ')
 endif
  NULLIFY(grid%spforcs4)
ENDIF
IF ( ASSOCIATED( grid%sp_amp4 ) ) THEN 
  DEALLOCATE(grid%sp_amp4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14549,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp4. ')
 endif
  NULLIFY(grid%sp_amp4)
ENDIF
IF ( ASSOCIATED( grid%spforcc5 ) ) THEN 
  DEALLOCATE(grid%spforcc5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14557,&
'frame/module_domain.f: Failed to deallocate grid%spforcc5. ')
 endif
  NULLIFY(grid%spforcc5)
ENDIF
IF ( ASSOCIATED( grid%spforcs5 ) ) THEN 
  DEALLOCATE(grid%spforcs5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14565,&
'frame/module_domain.f: Failed to deallocate grid%spforcs5. ')
 endif
  NULLIFY(grid%spforcs5)
ENDIF
IF ( ASSOCIATED( grid%sp_amp5 ) ) THEN 
  DEALLOCATE(grid%sp_amp5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14573,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp5. ')
 endif
  NULLIFY(grid%sp_amp5)
ENDIF
IF ( ASSOCIATED( grid%spptforcc ) ) THEN 
  DEALLOCATE(grid%spptforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14581,&
'frame/module_domain.f: Failed to deallocate grid%spptforcc. ')
 endif
  NULLIFY(grid%spptforcc)
ENDIF
IF ( ASSOCIATED( grid%spptforcs ) ) THEN 
  DEALLOCATE(grid%spptforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14589,&
'frame/module_domain.f: Failed to deallocate grid%spptforcs. ')
 endif
  NULLIFY(grid%spptforcs)
ENDIF
IF ( ASSOCIATED( grid%sppt_amp ) ) THEN 
  DEALLOCATE(grid%sppt_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14597,&
'frame/module_domain.f: Failed to deallocate grid%sppt_amp. ')
 endif
  NULLIFY(grid%sppt_amp)
ENDIF
IF ( ASSOCIATED( grid%vertampt ) ) THEN 
  DEALLOCATE(grid%vertampt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14605,&
'frame/module_domain.f: Failed to deallocate grid%vertampt. ')
 endif
  NULLIFY(grid%vertampt)
ENDIF
IF ( ASSOCIATED( grid%vertampuv ) ) THEN 
  DEALLOCATE(grid%vertampuv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14613,&
'frame/module_domain.f: Failed to deallocate grid%vertampuv. ')
 endif
  NULLIFY(grid%vertampuv)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_sppt ) ) THEN 
  DEALLOCATE(grid%iseedarr_sppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14621,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_sppt. ')
 endif
  NULLIFY(grid%iseedarr_sppt)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_skebs ) ) THEN 
  DEALLOCATE(grid%iseedarr_skebs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14629,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_skebs. ')
 endif
  NULLIFY(grid%iseedarr_skebs)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_rand_pert ) ) THEN 
  DEALLOCATE(grid%iseedarr_rand_pert,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14637,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_rand_pert. ')
 endif
  NULLIFY(grid%iseedarr_rand_pert)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_conv ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14645,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_conv. ')
 endif
  NULLIFY(grid%iseedarr_spp_conv)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_pbl ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14653,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_pbl. ')
 endif
  NULLIFY(grid%iseedarr_spp_pbl)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_lsm ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_lsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14661,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_lsm. ')
 endif
  NULLIFY(grid%iseedarr_spp_lsm)
ENDIF
IF ( ASSOCIATED( grid%rand_real_xxx ) ) THEN 
  DEALLOCATE(grid%rand_real_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14669,&
'frame/module_domain.f: Failed to deallocate grid%rand_real_xxx. ')
 endif
  NULLIFY(grid%rand_real_xxx)
ENDIF
IF ( ASSOCIATED( grid%rand_real_yyy ) ) THEN 
  DEALLOCATE(grid%rand_real_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14677,&
'frame/module_domain.f: Failed to deallocate grid%rand_real_yyy. ')
 endif
  NULLIFY(grid%rand_real_yyy)
ENDIF
IF ( ASSOCIATED( grid%rand_imag_xxx ) ) THEN 
  DEALLOCATE(grid%rand_imag_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14685,&
'frame/module_domain.f: Failed to deallocate grid%rand_imag_xxx. ')
 endif
  NULLIFY(grid%rand_imag_xxx)
ENDIF
IF ( ASSOCIATED( grid%rand_imag_yyy ) ) THEN 
  DEALLOCATE(grid%rand_imag_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14693,&
'frame/module_domain.f: Failed to deallocate grid%rand_imag_yyy. ')
 endif
  NULLIFY(grid%rand_imag_yyy)
ENDIF
IF ( ASSOCIATED( grid%gridpt_stddev_mult3d ) ) THEN 
  DEALLOCATE(grid%gridpt_stddev_mult3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14701,&
'frame/module_domain.f: Failed to deallocate grid%gridpt_stddev_mult3d. ')
 endif
  NULLIFY(grid%gridpt_stddev_mult3d)
ENDIF
IF ( ASSOCIATED( grid%stddev_cutoff_mult3d ) ) THEN 
  DEALLOCATE(grid%stddev_cutoff_mult3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14709,&
'frame/module_domain.f: Failed to deallocate grid%stddev_cutoff_mult3d. ')
 endif
  NULLIFY(grid%stddev_cutoff_mult3d)
ENDIF
IF ( ASSOCIATED( grid%lengthscale_mult3d ) ) THEN 
  DEALLOCATE(grid%lengthscale_mult3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14717,&
'frame/module_domain.f: Failed to deallocate grid%lengthscale_mult3d. ')
 endif
  NULLIFY(grid%lengthscale_mult3d)
ENDIF
IF ( ASSOCIATED( grid%timescale_mult3d ) ) THEN 
  DEALLOCATE(grid%timescale_mult3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14725,&
'frame/module_domain.f: Failed to deallocate grid%timescale_mult3d. ')
 endif
  NULLIFY(grid%timescale_mult3d)
ENDIF
IF ( ASSOCIATED( grid%mult3d_vertstruc ) ) THEN 
  DEALLOCATE(grid%mult3d_vertstruc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14733,&
'frame/module_domain.f: Failed to deallocate grid%mult3d_vertstruc. ')
 endif
  NULLIFY(grid%mult3d_vertstruc)
ENDIF
IF ( ASSOCIATED( grid%iseed_mult3d ) ) THEN 
  DEALLOCATE(grid%iseed_mult3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14741,&
'frame/module_domain.f: Failed to deallocate grid%iseed_mult3d. ')
 endif
  NULLIFY(grid%iseed_mult3d)
ENDIF
IF ( ASSOCIATED( grid%iseedarr_mult3d ) ) THEN 
  DEALLOCATE(grid%iseedarr_mult3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14749,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_mult3d. ')
 endif
  NULLIFY(grid%iseedarr_mult3d)
ENDIF
IF ( ASSOCIATED( grid%spforcc3d ) ) THEN 
  DEALLOCATE(grid%spforcc3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14757,&
'frame/module_domain.f: Failed to deallocate grid%spforcc3d. ')
 endif
  NULLIFY(grid%spforcc3d)
ENDIF
IF ( ASSOCIATED( grid%spforcs3d ) ) THEN 
  DEALLOCATE(grid%spforcs3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14765,&
'frame/module_domain.f: Failed to deallocate grid%spforcs3d. ')
 endif
  NULLIFY(grid%spforcs3d)
ENDIF
IF ( ASSOCIATED( grid%sp_amp3d ) ) THEN 
  DEALLOCATE(grid%sp_amp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14773,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp3d. ')
 endif
  NULLIFY(grid%sp_amp3d)
ENDIF
IF ( ASSOCIATED( grid%alph_rand3d ) ) THEN 
  DEALLOCATE(grid%alph_rand3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14781,&
'frame/module_domain.f: Failed to deallocate grid%alph_rand3d. ')
 endif
  NULLIFY(grid%alph_rand3d)
ENDIF
IF ( ASSOCIATED( grid%vertstrucc3d ) ) THEN 
  DEALLOCATE(grid%vertstrucc3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14789,&
'frame/module_domain.f: Failed to deallocate grid%vertstrucc3d. ')
 endif
  NULLIFY(grid%vertstrucc3d)
ENDIF
IF ( ASSOCIATED( grid%vertstrucs3d ) ) THEN 
  DEALLOCATE(grid%vertstrucs3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14797,&
'frame/module_domain.f: Failed to deallocate grid%vertstrucs3d. ')
 endif
  NULLIFY(grid%vertstrucs3d)
ENDIF
IF ( ASSOCIATED( grid%vertampt3d ) ) THEN 
  DEALLOCATE(grid%vertampt3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14805,&
'frame/module_domain.f: Failed to deallocate grid%vertampt3d. ')
 endif
  NULLIFY(grid%vertampt3d)
ENDIF
IF ( ASSOCIATED( grid%pert3d ) ) THEN 
  DEALLOCATE(grid%pert3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14813,&
'frame/module_domain.f: Failed to deallocate grid%pert3d. ')
 endif
  NULLIFY(grid%pert3d)
ENDIF
IF ( ASSOCIATED( grid%nba_mij ) ) THEN 
  DEALLOCATE(grid%nba_mij,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14821,&
'frame/module_domain.f: Failed to deallocate grid%nba_mij. ')
 endif
  NULLIFY(grid%nba_mij)
ENDIF
IF ( ASSOCIATED( grid%nba_rij ) ) THEN 
  DEALLOCATE(grid%nba_rij,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14829,&
'frame/module_domain.f: Failed to deallocate grid%nba_rij. ')
 endif
  NULLIFY(grid%nba_rij)
ENDIF
IF ( ASSOCIATED( grid%tauresx2d ) ) THEN 
  DEALLOCATE(grid%tauresx2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14837,&
'frame/module_domain.f: Failed to deallocate grid%tauresx2d. ')
 endif
  NULLIFY(grid%tauresx2d)
ENDIF
IF ( ASSOCIATED( grid%tauresy2d ) ) THEN 
  DEALLOCATE(grid%tauresy2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14845,&
'frame/module_domain.f: Failed to deallocate grid%tauresy2d. ')
 endif
  NULLIFY(grid%tauresy2d)
ENDIF
IF ( ASSOCIATED( grid%tpert2d ) ) THEN 
  DEALLOCATE(grid%tpert2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14853,&
'frame/module_domain.f: Failed to deallocate grid%tpert2d. ')
 endif
  NULLIFY(grid%tpert2d)
ENDIF
IF ( ASSOCIATED( grid%qpert2d ) ) THEN 
  DEALLOCATE(grid%qpert2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14861,&
'frame/module_domain.f: Failed to deallocate grid%qpert2d. ')
 endif
  NULLIFY(grid%qpert2d)
ENDIF
IF ( ASSOCIATED( grid%wpert2d ) ) THEN 
  DEALLOCATE(grid%wpert2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14869,&
'frame/module_domain.f: Failed to deallocate grid%wpert2d. ')
 endif
  NULLIFY(grid%wpert2d)
ENDIF
IF ( ASSOCIATED( grid%turbtype3d ) ) THEN 
  DEALLOCATE(grid%turbtype3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14877,&
'frame/module_domain.f: Failed to deallocate grid%turbtype3d. ')
 endif
  NULLIFY(grid%turbtype3d)
ENDIF
IF ( ASSOCIATED( grid%smaw3d ) ) THEN 
  DEALLOCATE(grid%smaw3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14885,&
'frame/module_domain.f: Failed to deallocate grid%smaw3d. ')
 endif
  NULLIFY(grid%smaw3d)
ENDIF
IF ( ASSOCIATED( grid%wsedl3d ) ) THEN 
  DEALLOCATE(grid%wsedl3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14893,&
'frame/module_domain.f: Failed to deallocate grid%wsedl3d. ')
 endif
  NULLIFY(grid%wsedl3d)
ENDIF
IF ( ASSOCIATED( grid%rliq ) ) THEN 
  DEALLOCATE(grid%rliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14901,&
'frame/module_domain.f: Failed to deallocate grid%rliq. ')
 endif
  NULLIFY(grid%rliq)
ENDIF
IF ( ASSOCIATED( grid%dlf ) ) THEN 
  DEALLOCATE(grid%dlf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14909,&
'frame/module_domain.f: Failed to deallocate grid%dlf. ')
 endif
  NULLIFY(grid%dlf)
ENDIF
IF ( ASSOCIATED( grid%precz ) ) THEN 
  DEALLOCATE(grid%precz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14917,&
'frame/module_domain.f: Failed to deallocate grid%precz. ')
 endif
  NULLIFY(grid%precz)
ENDIF
IF ( ASSOCIATED( grid%zmdt ) ) THEN 
  DEALLOCATE(grid%zmdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14925,&
'frame/module_domain.f: Failed to deallocate grid%zmdt. ')
 endif
  NULLIFY(grid%zmdt)
ENDIF
IF ( ASSOCIATED( grid%zmdq ) ) THEN 
  DEALLOCATE(grid%zmdq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14933,&
'frame/module_domain.f: Failed to deallocate grid%zmdq. ')
 endif
  NULLIFY(grid%zmdq)
ENDIF
IF ( ASSOCIATED( grid%zmdice ) ) THEN 
  DEALLOCATE(grid%zmdice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14941,&
'frame/module_domain.f: Failed to deallocate grid%zmdice. ')
 endif
  NULLIFY(grid%zmdice)
ENDIF
IF ( ASSOCIATED( grid%zmdliq ) ) THEN 
  DEALLOCATE(grid%zmdliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14949,&
'frame/module_domain.f: Failed to deallocate grid%zmdliq. ')
 endif
  NULLIFY(grid%zmdliq)
ENDIF
IF ( ASSOCIATED( grid%evaptzm ) ) THEN 
  DEALLOCATE(grid%evaptzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14957,&
'frame/module_domain.f: Failed to deallocate grid%evaptzm. ')
 endif
  NULLIFY(grid%evaptzm)
ENDIF
IF ( ASSOCIATED( grid%fzsntzm ) ) THEN 
  DEALLOCATE(grid%fzsntzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14965,&
'frame/module_domain.f: Failed to deallocate grid%fzsntzm. ')
 endif
  NULLIFY(grid%fzsntzm)
ENDIF
IF ( ASSOCIATED( grid%evsntzm ) ) THEN 
  DEALLOCATE(grid%evsntzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14973,&
'frame/module_domain.f: Failed to deallocate grid%evsntzm. ')
 endif
  NULLIFY(grid%evsntzm)
ENDIF
IF ( ASSOCIATED( grid%evapqzm ) ) THEN 
  DEALLOCATE(grid%evapqzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14981,&
'frame/module_domain.f: Failed to deallocate grid%evapqzm. ')
 endif
  NULLIFY(grid%evapqzm)
ENDIF
IF ( ASSOCIATED( grid%zmflxprc ) ) THEN 
  DEALLOCATE(grid%zmflxprc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14989,&
'frame/module_domain.f: Failed to deallocate grid%zmflxprc. ')
 endif
  NULLIFY(grid%zmflxprc)
ENDIF
IF ( ASSOCIATED( grid%zmflxsnw ) ) THEN 
  DEALLOCATE(grid%zmflxsnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14997,&
'frame/module_domain.f: Failed to deallocate grid%zmflxsnw. ')
 endif
  NULLIFY(grid%zmflxsnw)
ENDIF
IF ( ASSOCIATED( grid%zmntprpd ) ) THEN 
  DEALLOCATE(grid%zmntprpd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15005,&
'frame/module_domain.f: Failed to deallocate grid%zmntprpd. ')
 endif
  NULLIFY(grid%zmntprpd)
ENDIF
IF ( ASSOCIATED( grid%zmntsnpd ) ) THEN 
  DEALLOCATE(grid%zmntsnpd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15013,&
'frame/module_domain.f: Failed to deallocate grid%zmntsnpd. ')
 endif
  NULLIFY(grid%zmntsnpd)
ENDIF
IF ( ASSOCIATED( grid%zmeiheat ) ) THEN 
  DEALLOCATE(grid%zmeiheat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15021,&
'frame/module_domain.f: Failed to deallocate grid%zmeiheat. ')
 endif
  NULLIFY(grid%zmeiheat)
ENDIF
IF ( ASSOCIATED( grid%cmfmcdzm ) ) THEN 
  DEALLOCATE(grid%cmfmcdzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15029,&
'frame/module_domain.f: Failed to deallocate grid%cmfmcdzm. ')
 endif
  NULLIFY(grid%cmfmcdzm)
ENDIF
IF ( ASSOCIATED( grid%preccdzm ) ) THEN 
  DEALLOCATE(grid%preccdzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15037,&
'frame/module_domain.f: Failed to deallocate grid%preccdzm. ')
 endif
  NULLIFY(grid%preccdzm)
ENDIF
IF ( ASSOCIATED( grid%pconvb ) ) THEN 
  DEALLOCATE(grid%pconvb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15045,&
'frame/module_domain.f: Failed to deallocate grid%pconvb. ')
 endif
  NULLIFY(grid%pconvb)
ENDIF
IF ( ASSOCIATED( grid%pconvt ) ) THEN 
  DEALLOCATE(grid%pconvt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15053,&
'frame/module_domain.f: Failed to deallocate grid%pconvt. ')
 endif
  NULLIFY(grid%pconvt)
ENDIF
IF ( ASSOCIATED( grid%cape ) ) THEN 
  DEALLOCATE(grid%cape,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15061,&
'frame/module_domain.f: Failed to deallocate grid%cape. ')
 endif
  NULLIFY(grid%cape)
ENDIF
IF ( ASSOCIATED( grid%zmmtu ) ) THEN 
  DEALLOCATE(grid%zmmtu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15069,&
'frame/module_domain.f: Failed to deallocate grid%zmmtu. ')
 endif
  NULLIFY(grid%zmmtu)
ENDIF
IF ( ASSOCIATED( grid%zmmtv ) ) THEN 
  DEALLOCATE(grid%zmmtv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15077,&
'frame/module_domain.f: Failed to deallocate grid%zmmtv. ')
 endif
  NULLIFY(grid%zmmtv)
ENDIF
IF ( ASSOCIATED( grid%zmmu ) ) THEN 
  DEALLOCATE(grid%zmmu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15085,&
'frame/module_domain.f: Failed to deallocate grid%zmmu. ')
 endif
  NULLIFY(grid%zmmu)
ENDIF
IF ( ASSOCIATED( grid%zmmd ) ) THEN 
  DEALLOCATE(grid%zmmd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15093,&
'frame/module_domain.f: Failed to deallocate grid%zmmd. ')
 endif
  NULLIFY(grid%zmmd)
ENDIF
IF ( ASSOCIATED( grid%zmupgu ) ) THEN 
  DEALLOCATE(grid%zmupgu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15101,&
'frame/module_domain.f: Failed to deallocate grid%zmupgu. ')
 endif
  NULLIFY(grid%zmupgu)
ENDIF
IF ( ASSOCIATED( grid%zmupgd ) ) THEN 
  DEALLOCATE(grid%zmupgd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15109,&
'frame/module_domain.f: Failed to deallocate grid%zmupgd. ')
 endif
  NULLIFY(grid%zmupgd)
ENDIF
IF ( ASSOCIATED( grid%zmvpgu ) ) THEN 
  DEALLOCATE(grid%zmvpgu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15117,&
'frame/module_domain.f: Failed to deallocate grid%zmvpgu. ')
 endif
  NULLIFY(grid%zmvpgu)
ENDIF
IF ( ASSOCIATED( grid%zmvpgd ) ) THEN 
  DEALLOCATE(grid%zmvpgd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15125,&
'frame/module_domain.f: Failed to deallocate grid%zmvpgd. ')
 endif
  NULLIFY(grid%zmvpgd)
ENDIF
IF ( ASSOCIATED( grid%zmicuu ) ) THEN 
  DEALLOCATE(grid%zmicuu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15133,&
'frame/module_domain.f: Failed to deallocate grid%zmicuu. ')
 endif
  NULLIFY(grid%zmicuu)
ENDIF
IF ( ASSOCIATED( grid%zmicud ) ) THEN 
  DEALLOCATE(grid%zmicud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15141,&
'frame/module_domain.f: Failed to deallocate grid%zmicud. ')
 endif
  NULLIFY(grid%zmicud)
ENDIF
IF ( ASSOCIATED( grid%zmicvu ) ) THEN 
  DEALLOCATE(grid%zmicvu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15149,&
'frame/module_domain.f: Failed to deallocate grid%zmicvu. ')
 endif
  NULLIFY(grid%zmicvu)
ENDIF
IF ( ASSOCIATED( grid%zmicvd ) ) THEN 
  DEALLOCATE(grid%zmicvd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15157,&
'frame/module_domain.f: Failed to deallocate grid%zmicvd. ')
 endif
  NULLIFY(grid%zmicvd)
ENDIF
IF ( ASSOCIATED( grid%evapcdp3d ) ) THEN 
  DEALLOCATE(grid%evapcdp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15165,&
'frame/module_domain.f: Failed to deallocate grid%evapcdp3d. ')
 endif
  NULLIFY(grid%evapcdp3d)
ENDIF
IF ( ASSOCIATED( grid%icwmrdp3d ) ) THEN 
  DEALLOCATE(grid%icwmrdp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15173,&
'frame/module_domain.f: Failed to deallocate grid%icwmrdp3d. ')
 endif
  NULLIFY(grid%icwmrdp3d)
ENDIF
IF ( ASSOCIATED( grid%rprddp3d ) ) THEN 
  DEALLOCATE(grid%rprddp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15181,&
'frame/module_domain.f: Failed to deallocate grid%rprddp3d. ')
 endif
  NULLIFY(grid%rprddp3d)
ENDIF
IF ( ASSOCIATED( grid%dp3d ) ) THEN 
  DEALLOCATE(grid%dp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15189,&
'frame/module_domain.f: Failed to deallocate grid%dp3d. ')
 endif
  NULLIFY(grid%dp3d)
ENDIF
IF ( ASSOCIATED( grid%du3d ) ) THEN 
  DEALLOCATE(grid%du3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15197,&
'frame/module_domain.f: Failed to deallocate grid%du3d. ')
 endif
  NULLIFY(grid%du3d)
ENDIF
IF ( ASSOCIATED( grid%ed3d ) ) THEN 
  DEALLOCATE(grid%ed3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15205,&
'frame/module_domain.f: Failed to deallocate grid%ed3d. ')
 endif
  NULLIFY(grid%ed3d)
ENDIF
IF ( ASSOCIATED( grid%eu3d ) ) THEN 
  DEALLOCATE(grid%eu3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15213,&
'frame/module_domain.f: Failed to deallocate grid%eu3d. ')
 endif
  NULLIFY(grid%eu3d)
ENDIF
IF ( ASSOCIATED( grid%md3d ) ) THEN 
  DEALLOCATE(grid%md3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15221,&
'frame/module_domain.f: Failed to deallocate grid%md3d. ')
 endif
  NULLIFY(grid%md3d)
ENDIF
IF ( ASSOCIATED( grid%mu3d ) ) THEN 
  DEALLOCATE(grid%mu3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15229,&
'frame/module_domain.f: Failed to deallocate grid%mu3d. ')
 endif
  NULLIFY(grid%mu3d)
ENDIF
IF ( ASSOCIATED( grid%dsubcld2d ) ) THEN 
  DEALLOCATE(grid%dsubcld2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15237,&
'frame/module_domain.f: Failed to deallocate grid%dsubcld2d. ')
 endif
  NULLIFY(grid%dsubcld2d)
ENDIF
IF ( ASSOCIATED( grid%ideep2d ) ) THEN 
  DEALLOCATE(grid%ideep2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15245,&
'frame/module_domain.f: Failed to deallocate grid%ideep2d. ')
 endif
  NULLIFY(grid%ideep2d)
ENDIF
IF ( ASSOCIATED( grid%jt2d ) ) THEN 
  DEALLOCATE(grid%jt2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15253,&
'frame/module_domain.f: Failed to deallocate grid%jt2d. ')
 endif
  NULLIFY(grid%jt2d)
ENDIF
IF ( ASSOCIATED( grid%maxg2d ) ) THEN 
  DEALLOCATE(grid%maxg2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15261,&
'frame/module_domain.f: Failed to deallocate grid%maxg2d. ')
 endif
  NULLIFY(grid%maxg2d)
ENDIF
IF ( ASSOCIATED( grid%lengath2d ) ) THEN 
  DEALLOCATE(grid%lengath2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15269,&
'frame/module_domain.f: Failed to deallocate grid%lengath2d. ')
 endif
  NULLIFY(grid%lengath2d)
ENDIF
IF ( ASSOCIATED( grid%cmfsl ) ) THEN 
  DEALLOCATE(grid%cmfsl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15277,&
'frame/module_domain.f: Failed to deallocate grid%cmfsl. ')
 endif
  NULLIFY(grid%cmfsl)
ENDIF
IF ( ASSOCIATED( grid%cmflq ) ) THEN 
  DEALLOCATE(grid%cmflq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15285,&
'frame/module_domain.f: Failed to deallocate grid%cmflq. ')
 endif
  NULLIFY(grid%cmflq)
ENDIF
IF ( ASSOCIATED( grid%cmfmc ) ) THEN 
  DEALLOCATE(grid%cmfmc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15293,&
'frame/module_domain.f: Failed to deallocate grid%cmfmc. ')
 endif
  NULLIFY(grid%cmfmc)
ENDIF
IF ( ASSOCIATED( grid%cmfmc2 ) ) THEN 
  DEALLOCATE(grid%cmfmc2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15301,&
'frame/module_domain.f: Failed to deallocate grid%cmfmc2. ')
 endif
  NULLIFY(grid%cmfmc2)
ENDIF
IF ( ASSOCIATED( grid%cldfrash ) ) THEN 
  DEALLOCATE(grid%cldfrash,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15309,&
'frame/module_domain.f: Failed to deallocate grid%cldfrash. ')
 endif
  NULLIFY(grid%cldfrash)
ENDIF
IF ( ASSOCIATED( grid%cush ) ) THEN 
  DEALLOCATE(grid%cush,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15317,&
'frame/module_domain.f: Failed to deallocate grid%cush. ')
 endif
  NULLIFY(grid%cush)
ENDIF
IF ( ASSOCIATED( grid%evapcsh ) ) THEN 
  DEALLOCATE(grid%evapcsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15325,&
'frame/module_domain.f: Failed to deallocate grid%evapcsh. ')
 endif
  NULLIFY(grid%evapcsh)
ENDIF
IF ( ASSOCIATED( grid%icwmrsh ) ) THEN 
  DEALLOCATE(grid%icwmrsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15333,&
'frame/module_domain.f: Failed to deallocate grid%icwmrsh. ')
 endif
  NULLIFY(grid%icwmrsh)
ENDIF
IF ( ASSOCIATED( grid%snowsh ) ) THEN 
  DEALLOCATE(grid%snowsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15341,&
'frame/module_domain.f: Failed to deallocate grid%snowsh. ')
 endif
  NULLIFY(grid%snowsh)
ENDIF
IF ( ASSOCIATED( grid%rprdsh ) ) THEN 
  DEALLOCATE(grid%rprdsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15349,&
'frame/module_domain.f: Failed to deallocate grid%rprdsh. ')
 endif
  NULLIFY(grid%rprdsh)
ENDIF
IF ( ASSOCIATED( grid%rliq2 ) ) THEN 
  DEALLOCATE(grid%rliq2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15357,&
'frame/module_domain.f: Failed to deallocate grid%rliq2. ')
 endif
  NULLIFY(grid%rliq2)
ENDIF
IF ( ASSOCIATED( grid%dlf2 ) ) THEN 
  DEALLOCATE(grid%dlf2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15365,&
'frame/module_domain.f: Failed to deallocate grid%dlf2. ')
 endif
  NULLIFY(grid%dlf2)
ENDIF
IF ( ASSOCIATED( grid%shfrc3d ) ) THEN 
  DEALLOCATE(grid%shfrc3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15373,&
'frame/module_domain.f: Failed to deallocate grid%shfrc3d. ')
 endif
  NULLIFY(grid%shfrc3d)
ENDIF
IF ( ASSOCIATED( grid%qtflx_cu ) ) THEN 
  DEALLOCATE(grid%qtflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15381,&
'frame/module_domain.f: Failed to deallocate grid%qtflx_cu. ')
 endif
  NULLIFY(grid%qtflx_cu)
ENDIF
IF ( ASSOCIATED( grid%slflx_cu ) ) THEN 
  DEALLOCATE(grid%slflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15389,&
'frame/module_domain.f: Failed to deallocate grid%slflx_cu. ')
 endif
  NULLIFY(grid%slflx_cu)
ENDIF
IF ( ASSOCIATED( grid%uflx_cu ) ) THEN 
  DEALLOCATE(grid%uflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15397,&
'frame/module_domain.f: Failed to deallocate grid%uflx_cu. ')
 endif
  NULLIFY(grid%uflx_cu)
ENDIF
IF ( ASSOCIATED( grid%vflx_cu ) ) THEN 
  DEALLOCATE(grid%vflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15405,&
'frame/module_domain.f: Failed to deallocate grid%vflx_cu. ')
 endif
  NULLIFY(grid%vflx_cu)
ENDIF
IF ( ASSOCIATED( grid%qtten_cu ) ) THEN 
  DEALLOCATE(grid%qtten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15413,&
'frame/module_domain.f: Failed to deallocate grid%qtten_cu. ')
 endif
  NULLIFY(grid%qtten_cu)
ENDIF
IF ( ASSOCIATED( grid%slten_cu ) ) THEN 
  DEALLOCATE(grid%slten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15421,&
'frame/module_domain.f: Failed to deallocate grid%slten_cu. ')
 endif
  NULLIFY(grid%slten_cu)
ENDIF
IF ( ASSOCIATED( grid%uten_cu ) ) THEN 
  DEALLOCATE(grid%uten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15429,&
'frame/module_domain.f: Failed to deallocate grid%uten_cu. ')
 endif
  NULLIFY(grid%uten_cu)
ENDIF
IF ( ASSOCIATED( grid%vten_cu ) ) THEN 
  DEALLOCATE(grid%vten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15437,&
'frame/module_domain.f: Failed to deallocate grid%vten_cu. ')
 endif
  NULLIFY(grid%vten_cu)
ENDIF
IF ( ASSOCIATED( grid%qvten_cu ) ) THEN 
  DEALLOCATE(grid%qvten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15445,&
'frame/module_domain.f: Failed to deallocate grid%qvten_cu. ')
 endif
  NULLIFY(grid%qvten_cu)
ENDIF
IF ( ASSOCIATED( grid%qlten_cu ) ) THEN 
  DEALLOCATE(grid%qlten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15453,&
'frame/module_domain.f: Failed to deallocate grid%qlten_cu. ')
 endif
  NULLIFY(grid%qlten_cu)
ENDIF
IF ( ASSOCIATED( grid%qiten_cu ) ) THEN 
  DEALLOCATE(grid%qiten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15461,&
'frame/module_domain.f: Failed to deallocate grid%qiten_cu. ')
 endif
  NULLIFY(grid%qiten_cu)
ENDIF
IF ( ASSOCIATED( grid%cbmf_cu ) ) THEN 
  DEALLOCATE(grid%cbmf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15469,&
'frame/module_domain.f: Failed to deallocate grid%cbmf_cu. ')
 endif
  NULLIFY(grid%cbmf_cu)
ENDIF
IF ( ASSOCIATED( grid%ufrcinvbase_cu ) ) THEN 
  DEALLOCATE(grid%ufrcinvbase_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15477,&
'frame/module_domain.f: Failed to deallocate grid%ufrcinvbase_cu. ')
 endif
  NULLIFY(grid%ufrcinvbase_cu)
ENDIF
IF ( ASSOCIATED( grid%ufrclcl_cu ) ) THEN 
  DEALLOCATE(grid%ufrclcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15485,&
'frame/module_domain.f: Failed to deallocate grid%ufrclcl_cu. ')
 endif
  NULLIFY(grid%ufrclcl_cu)
ENDIF
IF ( ASSOCIATED( grid%winvbase_cu ) ) THEN 
  DEALLOCATE(grid%winvbase_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15493,&
'frame/module_domain.f: Failed to deallocate grid%winvbase_cu. ')
 endif
  NULLIFY(grid%winvbase_cu)
ENDIF
IF ( ASSOCIATED( grid%wlcl_cu ) ) THEN 
  DEALLOCATE(grid%wlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15501,&
'frame/module_domain.f: Failed to deallocate grid%wlcl_cu. ')
 endif
  NULLIFY(grid%wlcl_cu)
ENDIF
IF ( ASSOCIATED( grid%plcl_cu ) ) THEN 
  DEALLOCATE(grid%plcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15509,&
'frame/module_domain.f: Failed to deallocate grid%plcl_cu. ')
 endif
  NULLIFY(grid%plcl_cu)
ENDIF
IF ( ASSOCIATED( grid%pinv_cu ) ) THEN 
  DEALLOCATE(grid%pinv_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15517,&
'frame/module_domain.f: Failed to deallocate grid%pinv_cu. ')
 endif
  NULLIFY(grid%pinv_cu)
ENDIF
IF ( ASSOCIATED( grid%plfc_cu ) ) THEN 
  DEALLOCATE(grid%plfc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15525,&
'frame/module_domain.f: Failed to deallocate grid%plfc_cu. ')
 endif
  NULLIFY(grid%plfc_cu)
ENDIF
IF ( ASSOCIATED( grid%pbup_cu ) ) THEN 
  DEALLOCATE(grid%pbup_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15533,&
'frame/module_domain.f: Failed to deallocate grid%pbup_cu. ')
 endif
  NULLIFY(grid%pbup_cu)
ENDIF
IF ( ASSOCIATED( grid%ppen_cu ) ) THEN 
  DEALLOCATE(grid%ppen_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15541,&
'frame/module_domain.f: Failed to deallocate grid%ppen_cu. ')
 endif
  NULLIFY(grid%ppen_cu)
ENDIF
IF ( ASSOCIATED( grid%qtsrc_cu ) ) THEN 
  DEALLOCATE(grid%qtsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15549,&
'frame/module_domain.f: Failed to deallocate grid%qtsrc_cu. ')
 endif
  NULLIFY(grid%qtsrc_cu)
ENDIF
IF ( ASSOCIATED( grid%thlsrc_cu ) ) THEN 
  DEALLOCATE(grid%thlsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15557,&
'frame/module_domain.f: Failed to deallocate grid%thlsrc_cu. ')
 endif
  NULLIFY(grid%thlsrc_cu)
ENDIF
IF ( ASSOCIATED( grid%thvlsrc_cu ) ) THEN 
  DEALLOCATE(grid%thvlsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15565,&
'frame/module_domain.f: Failed to deallocate grid%thvlsrc_cu. ')
 endif
  NULLIFY(grid%thvlsrc_cu)
ENDIF
IF ( ASSOCIATED( grid%emkfbup_cu ) ) THEN 
  DEALLOCATE(grid%emkfbup_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15573,&
'frame/module_domain.f: Failed to deallocate grid%emkfbup_cu. ')
 endif
  NULLIFY(grid%emkfbup_cu)
ENDIF
IF ( ASSOCIATED( grid%cin_cu ) ) THEN 
  DEALLOCATE(grid%cin_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15581,&
'frame/module_domain.f: Failed to deallocate grid%cin_cu. ')
 endif
  NULLIFY(grid%cin_cu)
ENDIF
IF ( ASSOCIATED( grid%cinlcl_cu ) ) THEN 
  DEALLOCATE(grid%cinlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15589,&
'frame/module_domain.f: Failed to deallocate grid%cinlcl_cu. ')
 endif
  NULLIFY(grid%cinlcl_cu)
ENDIF
IF ( ASSOCIATED( grid%cbmflimit_cu ) ) THEN 
  DEALLOCATE(grid%cbmflimit_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15597,&
'frame/module_domain.f: Failed to deallocate grid%cbmflimit_cu. ')
 endif
  NULLIFY(grid%cbmflimit_cu)
ENDIF
IF ( ASSOCIATED( grid%tkeavg_cu ) ) THEN 
  DEALLOCATE(grid%tkeavg_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15605,&
'frame/module_domain.f: Failed to deallocate grid%tkeavg_cu. ')
 endif
  NULLIFY(grid%tkeavg_cu)
ENDIF
IF ( ASSOCIATED( grid%zinv_cu ) ) THEN 
  DEALLOCATE(grid%zinv_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15613,&
'frame/module_domain.f: Failed to deallocate grid%zinv_cu. ')
 endif
  NULLIFY(grid%zinv_cu)
ENDIF
IF ( ASSOCIATED( grid%rcwp_cu ) ) THEN 
  DEALLOCATE(grid%rcwp_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15621,&
'frame/module_domain.f: Failed to deallocate grid%rcwp_cu. ')
 endif
  NULLIFY(grid%rcwp_cu)
ENDIF
IF ( ASSOCIATED( grid%rlwp_cu ) ) THEN 
  DEALLOCATE(grid%rlwp_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15629,&
'frame/module_domain.f: Failed to deallocate grid%rlwp_cu. ')
 endif
  NULLIFY(grid%rlwp_cu)
ENDIF
IF ( ASSOCIATED( grid%riwp_cu ) ) THEN 
  DEALLOCATE(grid%riwp_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15637,&
'frame/module_domain.f: Failed to deallocate grid%riwp_cu. ')
 endif
  NULLIFY(grid%riwp_cu)
ENDIF
IF ( ASSOCIATED( grid%tophgt_cu ) ) THEN 
  DEALLOCATE(grid%tophgt_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15645,&
'frame/module_domain.f: Failed to deallocate grid%tophgt_cu. ')
 endif
  NULLIFY(grid%tophgt_cu)
ENDIF
IF ( ASSOCIATED( grid%wu_cu ) ) THEN 
  DEALLOCATE(grid%wu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15653,&
'frame/module_domain.f: Failed to deallocate grid%wu_cu. ')
 endif
  NULLIFY(grid%wu_cu)
ENDIF
IF ( ASSOCIATED( grid%ufrc_cu ) ) THEN 
  DEALLOCATE(grid%ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15661,&
'frame/module_domain.f: Failed to deallocate grid%ufrc_cu. ')
 endif
  NULLIFY(grid%ufrc_cu)
ENDIF
IF ( ASSOCIATED( grid%qtu_cu ) ) THEN 
  DEALLOCATE(grid%qtu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15669,&
'frame/module_domain.f: Failed to deallocate grid%qtu_cu. ')
 endif
  NULLIFY(grid%qtu_cu)
ENDIF
IF ( ASSOCIATED( grid%thlu_cu ) ) THEN 
  DEALLOCATE(grid%thlu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15677,&
'frame/module_domain.f: Failed to deallocate grid%thlu_cu. ')
 endif
  NULLIFY(grid%thlu_cu)
ENDIF
IF ( ASSOCIATED( grid%thvu_cu ) ) THEN 
  DEALLOCATE(grid%thvu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15685,&
'frame/module_domain.f: Failed to deallocate grid%thvu_cu. ')
 endif
  NULLIFY(grid%thvu_cu)
ENDIF
IF ( ASSOCIATED( grid%uu_cu ) ) THEN 
  DEALLOCATE(grid%uu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15693,&
'frame/module_domain.f: Failed to deallocate grid%uu_cu. ')
 endif
  NULLIFY(grid%uu_cu)
ENDIF
IF ( ASSOCIATED( grid%vu_cu ) ) THEN 
  DEALLOCATE(grid%vu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15701,&
'frame/module_domain.f: Failed to deallocate grid%vu_cu. ')
 endif
  NULLIFY(grid%vu_cu)
ENDIF
IF ( ASSOCIATED( grid%qtu_emf_cu ) ) THEN 
  DEALLOCATE(grid%qtu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15709,&
'frame/module_domain.f: Failed to deallocate grid%qtu_emf_cu. ')
 endif
  NULLIFY(grid%qtu_emf_cu)
ENDIF
IF ( ASSOCIATED( grid%thlu_emf_cu ) ) THEN 
  DEALLOCATE(grid%thlu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15717,&
'frame/module_domain.f: Failed to deallocate grid%thlu_emf_cu. ')
 endif
  NULLIFY(grid%thlu_emf_cu)
ENDIF
IF ( ASSOCIATED( grid%uu_emf_cu ) ) THEN 
  DEALLOCATE(grid%uu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15725,&
'frame/module_domain.f: Failed to deallocate grid%uu_emf_cu. ')
 endif
  NULLIFY(grid%uu_emf_cu)
ENDIF
IF ( ASSOCIATED( grid%vu_emf_cu ) ) THEN 
  DEALLOCATE(grid%vu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15733,&
'frame/module_domain.f: Failed to deallocate grid%vu_emf_cu. ')
 endif
  NULLIFY(grid%vu_emf_cu)
ENDIF
IF ( ASSOCIATED( grid%umf_cu ) ) THEN 
  DEALLOCATE(grid%umf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15741,&
'frame/module_domain.f: Failed to deallocate grid%umf_cu. ')
 endif
  NULLIFY(grid%umf_cu)
ENDIF
IF ( ASSOCIATED( grid%uemf_cu ) ) THEN 
  DEALLOCATE(grid%uemf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15749,&
'frame/module_domain.f: Failed to deallocate grid%uemf_cu. ')
 endif
  NULLIFY(grid%uemf_cu)
ENDIF
IF ( ASSOCIATED( grid%qcu_cu ) ) THEN 
  DEALLOCATE(grid%qcu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15757,&
'frame/module_domain.f: Failed to deallocate grid%qcu_cu. ')
 endif
  NULLIFY(grid%qcu_cu)
ENDIF
IF ( ASSOCIATED( grid%qlu_cu ) ) THEN 
  DEALLOCATE(grid%qlu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15765,&
'frame/module_domain.f: Failed to deallocate grid%qlu_cu. ')
 endif
  NULLIFY(grid%qlu_cu)
ENDIF
IF ( ASSOCIATED( grid%qiu_cu ) ) THEN 
  DEALLOCATE(grid%qiu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15773,&
'frame/module_domain.f: Failed to deallocate grid%qiu_cu. ')
 endif
  NULLIFY(grid%qiu_cu)
ENDIF
IF ( ASSOCIATED( grid%cufrc_cu ) ) THEN 
  DEALLOCATE(grid%cufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15781,&
'frame/module_domain.f: Failed to deallocate grid%cufrc_cu. ')
 endif
  NULLIFY(grid%cufrc_cu)
ENDIF
IF ( ASSOCIATED( grid%fer_cu ) ) THEN 
  DEALLOCATE(grid%fer_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15789,&
'frame/module_domain.f: Failed to deallocate grid%fer_cu. ')
 endif
  NULLIFY(grid%fer_cu)
ENDIF
IF ( ASSOCIATED( grid%fdr_cu ) ) THEN 
  DEALLOCATE(grid%fdr_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15797,&
'frame/module_domain.f: Failed to deallocate grid%fdr_cu. ')
 endif
  NULLIFY(grid%fdr_cu)
ENDIF
IF ( ASSOCIATED( grid%dwten_cu ) ) THEN 
  DEALLOCATE(grid%dwten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15805,&
'frame/module_domain.f: Failed to deallocate grid%dwten_cu. ')
 endif
  NULLIFY(grid%dwten_cu)
ENDIF
IF ( ASSOCIATED( grid%diten_cu ) ) THEN 
  DEALLOCATE(grid%diten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15813,&
'frame/module_domain.f: Failed to deallocate grid%diten_cu. ')
 endif
  NULLIFY(grid%diten_cu)
ENDIF
IF ( ASSOCIATED( grid%qrten_cu ) ) THEN 
  DEALLOCATE(grid%qrten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15821,&
'frame/module_domain.f: Failed to deallocate grid%qrten_cu. ')
 endif
  NULLIFY(grid%qrten_cu)
ENDIF
IF ( ASSOCIATED( grid%qsten_cu ) ) THEN 
  DEALLOCATE(grid%qsten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15829,&
'frame/module_domain.f: Failed to deallocate grid%qsten_cu. ')
 endif
  NULLIFY(grid%qsten_cu)
ENDIF
IF ( ASSOCIATED( grid%flxrain_cu ) ) THEN 
  DEALLOCATE(grid%flxrain_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15837,&
'frame/module_domain.f: Failed to deallocate grid%flxrain_cu. ')
 endif
  NULLIFY(grid%flxrain_cu)
ENDIF
IF ( ASSOCIATED( grid%flxsnow_cu ) ) THEN 
  DEALLOCATE(grid%flxsnow_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15845,&
'frame/module_domain.f: Failed to deallocate grid%flxsnow_cu. ')
 endif
  NULLIFY(grid%flxsnow_cu)
ENDIF
IF ( ASSOCIATED( grid%ntraprd_cu ) ) THEN 
  DEALLOCATE(grid%ntraprd_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15853,&
'frame/module_domain.f: Failed to deallocate grid%ntraprd_cu. ')
 endif
  NULLIFY(grid%ntraprd_cu)
ENDIF
IF ( ASSOCIATED( grid%ntsnprd_cu ) ) THEN 
  DEALLOCATE(grid%ntsnprd_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15861,&
'frame/module_domain.f: Failed to deallocate grid%ntsnprd_cu. ')
 endif
  NULLIFY(grid%ntsnprd_cu)
ENDIF
IF ( ASSOCIATED( grid%excessu_cu ) ) THEN 
  DEALLOCATE(grid%excessu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15869,&
'frame/module_domain.f: Failed to deallocate grid%excessu_cu. ')
 endif
  NULLIFY(grid%excessu_cu)
ENDIF
IF ( ASSOCIATED( grid%excessu0_cu ) ) THEN 
  DEALLOCATE(grid%excessu0_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15877,&
'frame/module_domain.f: Failed to deallocate grid%excessu0_cu. ')
 endif
  NULLIFY(grid%excessu0_cu)
ENDIF
IF ( ASSOCIATED( grid%xc_cu ) ) THEN 
  DEALLOCATE(grid%xc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15885,&
'frame/module_domain.f: Failed to deallocate grid%xc_cu. ')
 endif
  NULLIFY(grid%xc_cu)
ENDIF
IF ( ASSOCIATED( grid%aquad_cu ) ) THEN 
  DEALLOCATE(grid%aquad_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15893,&
'frame/module_domain.f: Failed to deallocate grid%aquad_cu. ')
 endif
  NULLIFY(grid%aquad_cu)
ENDIF
IF ( ASSOCIATED( grid%bquad_cu ) ) THEN 
  DEALLOCATE(grid%bquad_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15901,&
'frame/module_domain.f: Failed to deallocate grid%bquad_cu. ')
 endif
  NULLIFY(grid%bquad_cu)
ENDIF
IF ( ASSOCIATED( grid%cquad_cu ) ) THEN 
  DEALLOCATE(grid%cquad_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15909,&
'frame/module_domain.f: Failed to deallocate grid%cquad_cu. ')
 endif
  NULLIFY(grid%cquad_cu)
ENDIF
IF ( ASSOCIATED( grid%bogbot_cu ) ) THEN 
  DEALLOCATE(grid%bogbot_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15917,&
'frame/module_domain.f: Failed to deallocate grid%bogbot_cu. ')
 endif
  NULLIFY(grid%bogbot_cu)
ENDIF
IF ( ASSOCIATED( grid%bogtop_cu ) ) THEN 
  DEALLOCATE(grid%bogtop_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15925,&
'frame/module_domain.f: Failed to deallocate grid%bogtop_cu. ')
 endif
  NULLIFY(grid%bogtop_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_uwcu_cu ) ) THEN 
  DEALLOCATE(grid%exit_uwcu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15933,&
'frame/module_domain.f: Failed to deallocate grid%exit_uwcu_cu. ')
 endif
  NULLIFY(grid%exit_uwcu_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_conden_cu ) ) THEN 
  DEALLOCATE(grid%exit_conden_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15941,&
'frame/module_domain.f: Failed to deallocate grid%exit_conden_cu. ')
 endif
  NULLIFY(grid%exit_conden_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_klclmkx_cu ) ) THEN 
  DEALLOCATE(grid%exit_klclmkx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15949,&
'frame/module_domain.f: Failed to deallocate grid%exit_klclmkx_cu. ')
 endif
  NULLIFY(grid%exit_klclmkx_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_klfcmkx_cu ) ) THEN 
  DEALLOCATE(grid%exit_klfcmkx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15957,&
'frame/module_domain.f: Failed to deallocate grid%exit_klfcmkx_cu. ')
 endif
  NULLIFY(grid%exit_klfcmkx_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_ufrc_cu ) ) THEN 
  DEALLOCATE(grid%exit_ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15965,&
'frame/module_domain.f: Failed to deallocate grid%exit_ufrc_cu. ')
 endif
  NULLIFY(grid%exit_ufrc_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_wtw_cu ) ) THEN 
  DEALLOCATE(grid%exit_wtw_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15973,&
'frame/module_domain.f: Failed to deallocate grid%exit_wtw_cu. ')
 endif
  NULLIFY(grid%exit_wtw_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_drycore_cu ) ) THEN 
  DEALLOCATE(grid%exit_drycore_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15981,&
'frame/module_domain.f: Failed to deallocate grid%exit_drycore_cu. ')
 endif
  NULLIFY(grid%exit_drycore_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_wu_cu ) ) THEN 
  DEALLOCATE(grid%exit_wu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15989,&
'frame/module_domain.f: Failed to deallocate grid%exit_wu_cu. ')
 endif
  NULLIFY(grid%exit_wu_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_cufliter_cu ) ) THEN 
  DEALLOCATE(grid%exit_cufliter_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15997,&
'frame/module_domain.f: Failed to deallocate grid%exit_cufliter_cu. ')
 endif
  NULLIFY(grid%exit_cufliter_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_kinv1_cu ) ) THEN 
  DEALLOCATE(grid%exit_kinv1_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16005,&
'frame/module_domain.f: Failed to deallocate grid%exit_kinv1_cu. ')
 endif
  NULLIFY(grid%exit_kinv1_cu)
ENDIF
IF ( ASSOCIATED( grid%exit_rei_cu ) ) THEN 
  DEALLOCATE(grid%exit_rei_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16013,&
'frame/module_domain.f: Failed to deallocate grid%exit_rei_cu. ')
 endif
  NULLIFY(grid%exit_rei_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_shcu_cu ) ) THEN 
  DEALLOCATE(grid%limit_shcu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16021,&
'frame/module_domain.f: Failed to deallocate grid%limit_shcu_cu. ')
 endif
  NULLIFY(grid%limit_shcu_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_negcon_cu ) ) THEN 
  DEALLOCATE(grid%limit_negcon_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16029,&
'frame/module_domain.f: Failed to deallocate grid%limit_negcon_cu. ')
 endif
  NULLIFY(grid%limit_negcon_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_ufrc_cu ) ) THEN 
  DEALLOCATE(grid%limit_ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16037,&
'frame/module_domain.f: Failed to deallocate grid%limit_ufrc_cu. ')
 endif
  NULLIFY(grid%limit_ufrc_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_ppen_cu ) ) THEN 
  DEALLOCATE(grid%limit_ppen_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16045,&
'frame/module_domain.f: Failed to deallocate grid%limit_ppen_cu. ')
 endif
  NULLIFY(grid%limit_ppen_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_emf_cu ) ) THEN 
  DEALLOCATE(grid%limit_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16053,&
'frame/module_domain.f: Failed to deallocate grid%limit_emf_cu. ')
 endif
  NULLIFY(grid%limit_emf_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_cinlcl_cu ) ) THEN 
  DEALLOCATE(grid%limit_cinlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16061,&
'frame/module_domain.f: Failed to deallocate grid%limit_cinlcl_cu. ')
 endif
  NULLIFY(grid%limit_cinlcl_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_cin_cu ) ) THEN 
  DEALLOCATE(grid%limit_cin_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16069,&
'frame/module_domain.f: Failed to deallocate grid%limit_cin_cu. ')
 endif
  NULLIFY(grid%limit_cin_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_cbmf_cu ) ) THEN 
  DEALLOCATE(grid%limit_cbmf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16077,&
'frame/module_domain.f: Failed to deallocate grid%limit_cbmf_cu. ')
 endif
  NULLIFY(grid%limit_cbmf_cu)
ENDIF
IF ( ASSOCIATED( grid%limit_rei_cu ) ) THEN 
  DEALLOCATE(grid%limit_rei_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16085,&
'frame/module_domain.f: Failed to deallocate grid%limit_rei_cu. ')
 endif
  NULLIFY(grid%limit_rei_cu)
ENDIF
IF ( ASSOCIATED( grid%ind_delcin_cu ) ) THEN 
  DEALLOCATE(grid%ind_delcin_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16093,&
'frame/module_domain.f: Failed to deallocate grid%ind_delcin_cu. ')
 endif
  NULLIFY(grid%ind_delcin_cu)
ENDIF
IF ( ASSOCIATED( grid%rh_old_mp ) ) THEN 
  DEALLOCATE(grid%rh_old_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16101,&
'frame/module_domain.f: Failed to deallocate grid%rh_old_mp. ')
 endif
  NULLIFY(grid%rh_old_mp)
ENDIF
IF ( ASSOCIATED( grid%lcd_old_mp ) ) THEN 
  DEALLOCATE(grid%lcd_old_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16109,&
'frame/module_domain.f: Failed to deallocate grid%lcd_old_mp. ')
 endif
  NULLIFY(grid%lcd_old_mp)
ENDIF
IF ( ASSOCIATED( grid%cldfra_old_mp ) ) THEN 
  DEALLOCATE(grid%cldfra_old_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16117,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_old_mp. ')
 endif
  NULLIFY(grid%cldfra_old_mp)
ENDIF
IF ( ASSOCIATED( grid%cldfra_mp ) ) THEN 
  DEALLOCATE(grid%cldfra_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16125,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_mp. ')
 endif
  NULLIFY(grid%cldfra_mp)
ENDIF
IF ( ASSOCIATED( grid%cldfra_mp_all ) ) THEN 
  DEALLOCATE(grid%cldfra_mp_all,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16133,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_mp_all. ')
 endif
  NULLIFY(grid%cldfra_mp_all)
ENDIF
IF ( ASSOCIATED( grid%iradius ) ) THEN 
  DEALLOCATE(grid%iradius,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16141,&
'frame/module_domain.f: Failed to deallocate grid%iradius. ')
 endif
  NULLIFY(grid%iradius)
ENDIF
IF ( ASSOCIATED( grid%lradius ) ) THEN 
  DEALLOCATE(grid%lradius,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16149,&
'frame/module_domain.f: Failed to deallocate grid%lradius. ')
 endif
  NULLIFY(grid%lradius)
ENDIF
IF ( ASSOCIATED( grid%cldfra_conv ) ) THEN 
  DEALLOCATE(grid%cldfra_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16157,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_conv. ')
 endif
  NULLIFY(grid%cldfra_conv)
ENDIF
IF ( ASSOCIATED( grid%cldfrai ) ) THEN 
  DEALLOCATE(grid%cldfrai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16165,&
'frame/module_domain.f: Failed to deallocate grid%cldfrai. ')
 endif
  NULLIFY(grid%cldfrai)
ENDIF
IF ( ASSOCIATED( grid%cldfral ) ) THEN 
  DEALLOCATE(grid%cldfral,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16173,&
'frame/module_domain.f: Failed to deallocate grid%cldfral. ')
 endif
  NULLIFY(grid%cldfral)
ENDIF
IF ( ASSOCIATED( grid%numc ) ) THEN 
  DEALLOCATE(grid%numc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16181,&
'frame/module_domain.f: Failed to deallocate grid%numc. ')
 endif
  NULLIFY(grid%numc)
ENDIF
IF ( ASSOCIATED( grid%nump ) ) THEN 
  DEALLOCATE(grid%nump,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16189,&
'frame/module_domain.f: Failed to deallocate grid%nump. ')
 endif
  NULLIFY(grid%nump)
ENDIF
IF ( ASSOCIATED( grid%sabv ) ) THEN 
  DEALLOCATE(grid%sabv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16197,&
'frame/module_domain.f: Failed to deallocate grid%sabv. ')
 endif
  NULLIFY(grid%sabv)
ENDIF
IF ( ASSOCIATED( grid%sabg ) ) THEN 
  DEALLOCATE(grid%sabg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16205,&
'frame/module_domain.f: Failed to deallocate grid%sabg. ')
 endif
  NULLIFY(grid%sabg)
ENDIF
IF ( ASSOCIATED( grid%lwup ) ) THEN 
  DEALLOCATE(grid%lwup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16213,&
'frame/module_domain.f: Failed to deallocate grid%lwup. ')
 endif
  NULLIFY(grid%lwup)
ENDIF
IF ( ASSOCIATED( grid%lhsoi ) ) THEN 
  DEALLOCATE(grid%lhsoi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16221,&
'frame/module_domain.f: Failed to deallocate grid%lhsoi. ')
 endif
  NULLIFY(grid%lhsoi)
ENDIF
IF ( ASSOCIATED( grid%lhveg ) ) THEN 
  DEALLOCATE(grid%lhveg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16229,&
'frame/module_domain.f: Failed to deallocate grid%lhveg. ')
 endif
  NULLIFY(grid%lhveg)
ENDIF
IF ( ASSOCIATED( grid%lhtran ) ) THEN 
  DEALLOCATE(grid%lhtran,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16237,&
'frame/module_domain.f: Failed to deallocate grid%lhtran. ')
 endif
  NULLIFY(grid%lhtran)
ENDIF
IF ( ASSOCIATED( grid%snl ) ) THEN 
  DEALLOCATE(grid%snl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16245,&
'frame/module_domain.f: Failed to deallocate grid%snl. ')
 endif
  NULLIFY(grid%snl)
ENDIF
IF ( ASSOCIATED( grid%snowdp ) ) THEN 
  DEALLOCATE(grid%snowdp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16253,&
'frame/module_domain.f: Failed to deallocate grid%snowdp. ')
 endif
  NULLIFY(grid%snowdp)
ENDIF
IF ( ASSOCIATED( grid%wtc ) ) THEN 
  DEALLOCATE(grid%wtc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16261,&
'frame/module_domain.f: Failed to deallocate grid%wtc. ')
 endif
  NULLIFY(grid%wtc)
ENDIF
IF ( ASSOCIATED( grid%wtp ) ) THEN 
  DEALLOCATE(grid%wtp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16269,&
'frame/module_domain.f: Failed to deallocate grid%wtp. ')
 endif
  NULLIFY(grid%wtp)
ENDIF
IF ( ASSOCIATED( grid%h2osno ) ) THEN 
  DEALLOCATE(grid%h2osno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16277,&
'frame/module_domain.f: Failed to deallocate grid%h2osno. ')
 endif
  NULLIFY(grid%h2osno)
ENDIF
IF ( ASSOCIATED( grid%t_grnd ) ) THEN 
  DEALLOCATE(grid%t_grnd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16285,&
'frame/module_domain.f: Failed to deallocate grid%t_grnd. ')
 endif
  NULLIFY(grid%t_grnd)
ENDIF
IF ( ASSOCIATED( grid%t_veg ) ) THEN 
  DEALLOCATE(grid%t_veg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16293,&
'frame/module_domain.f: Failed to deallocate grid%t_veg. ')
 endif
  NULLIFY(grid%t_veg)
ENDIF
IF ( ASSOCIATED( grid%t_veg24 ) ) THEN 
  DEALLOCATE(grid%t_veg24,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16301,&
'frame/module_domain.f: Failed to deallocate grid%t_veg24. ')
 endif
  NULLIFY(grid%t_veg24)
ENDIF
IF ( ASSOCIATED( grid%t_veg240 ) ) THEN 
  DEALLOCATE(grid%t_veg240,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16309,&
'frame/module_domain.f: Failed to deallocate grid%t_veg240. ')
 endif
  NULLIFY(grid%t_veg240)
ENDIF
IF ( ASSOCIATED( grid%fsun ) ) THEN 
  DEALLOCATE(grid%fsun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16317,&
'frame/module_domain.f: Failed to deallocate grid%fsun. ')
 endif
  NULLIFY(grid%fsun)
ENDIF
IF ( ASSOCIATED( grid%fsun24 ) ) THEN 
  DEALLOCATE(grid%fsun24,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16325,&
'frame/module_domain.f: Failed to deallocate grid%fsun24. ')
 endif
  NULLIFY(grid%fsun24)
ENDIF
IF ( ASSOCIATED( grid%fsun240 ) ) THEN 
  DEALLOCATE(grid%fsun240,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16333,&
'frame/module_domain.f: Failed to deallocate grid%fsun240. ')
 endif
  NULLIFY(grid%fsun240)
ENDIF
IF ( ASSOCIATED( grid%fsd24 ) ) THEN 
  DEALLOCATE(grid%fsd24,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16341,&
'frame/module_domain.f: Failed to deallocate grid%fsd24. ')
 endif
  NULLIFY(grid%fsd24)
ENDIF
IF ( ASSOCIATED( grid%fsd240 ) ) THEN 
  DEALLOCATE(grid%fsd240,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16349,&
'frame/module_domain.f: Failed to deallocate grid%fsd240. ')
 endif
  NULLIFY(grid%fsd240)
ENDIF
IF ( ASSOCIATED( grid%fsi24 ) ) THEN 
  DEALLOCATE(grid%fsi24,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16357,&
'frame/module_domain.f: Failed to deallocate grid%fsi24. ')
 endif
  NULLIFY(grid%fsi24)
ENDIF
IF ( ASSOCIATED( grid%fsi240 ) ) THEN 
  DEALLOCATE(grid%fsi240,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16365,&
'frame/module_domain.f: Failed to deallocate grid%fsi240. ')
 endif
  NULLIFY(grid%fsi240)
ENDIF
IF ( ASSOCIATED( grid%laip ) ) THEN 
  DEALLOCATE(grid%laip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16373,&
'frame/module_domain.f: Failed to deallocate grid%laip. ')
 endif
  NULLIFY(grid%laip)
ENDIF
IF ( ASSOCIATED( grid%h2ocan ) ) THEN 
  DEALLOCATE(grid%h2ocan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16381,&
'frame/module_domain.f: Failed to deallocate grid%h2ocan. ')
 endif
  NULLIFY(grid%h2ocan)
ENDIF
IF ( ASSOCIATED( grid%h2ocan_col ) ) THEN 
  DEALLOCATE(grid%h2ocan_col,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16389,&
'frame/module_domain.f: Failed to deallocate grid%h2ocan_col. ')
 endif
  NULLIFY(grid%h2ocan_col)
ENDIF
IF ( ASSOCIATED( grid%t2m_max ) ) THEN 
  DEALLOCATE(grid%t2m_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16397,&
'frame/module_domain.f: Failed to deallocate grid%t2m_max. ')
 endif
  NULLIFY(grid%t2m_max)
ENDIF
IF ( ASSOCIATED( grid%t2m_min ) ) THEN 
  DEALLOCATE(grid%t2m_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16405,&
'frame/module_domain.f: Failed to deallocate grid%t2m_min. ')
 endif
  NULLIFY(grid%t2m_min)
ENDIF
IF ( ASSOCIATED( grid%t2clm ) ) THEN 
  DEALLOCATE(grid%t2clm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16413,&
'frame/module_domain.f: Failed to deallocate grid%t2clm. ')
 endif
  NULLIFY(grid%t2clm)
ENDIF
IF ( ASSOCIATED( grid%t_ref2m ) ) THEN 
  DEALLOCATE(grid%t_ref2m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16421,&
'frame/module_domain.f: Failed to deallocate grid%t_ref2m. ')
 endif
  NULLIFY(grid%t_ref2m)
ENDIF
IF ( ASSOCIATED( grid%q_ref2m ) ) THEN 
  DEALLOCATE(grid%q_ref2m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16429,&
'frame/module_domain.f: Failed to deallocate grid%q_ref2m. ')
 endif
  NULLIFY(grid%q_ref2m)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16437,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s1. ')
 endif
  NULLIFY(grid%h2osoi_liq_s1)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16445,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s2. ')
 endif
  NULLIFY(grid%h2osoi_liq_s2)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16453,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s3. ')
 endif
  NULLIFY(grid%h2osoi_liq_s3)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16461,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s4. ')
 endif
  NULLIFY(grid%h2osoi_liq_s4)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16469,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s5. ')
 endif
  NULLIFY(grid%h2osoi_liq_s5)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16477,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq1. ')
 endif
  NULLIFY(grid%h2osoi_liq1)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16485,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq2. ')
 endif
  NULLIFY(grid%h2osoi_liq2)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16493,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq3. ')
 endif
  NULLIFY(grid%h2osoi_liq3)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16501,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq4. ')
 endif
  NULLIFY(grid%h2osoi_liq4)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16509,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq5. ')
 endif
  NULLIFY(grid%h2osoi_liq5)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16517,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq6. ')
 endif
  NULLIFY(grid%h2osoi_liq6)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16525,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq7. ')
 endif
  NULLIFY(grid%h2osoi_liq7)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16533,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq8. ')
 endif
  NULLIFY(grid%h2osoi_liq8)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16541,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq9. ')
 endif
  NULLIFY(grid%h2osoi_liq9)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16549,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq10. ')
 endif
  NULLIFY(grid%h2osoi_liq10)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16557,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s1. ')
 endif
  NULLIFY(grid%h2osoi_ice_s1)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16565,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s2. ')
 endif
  NULLIFY(grid%h2osoi_ice_s2)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16573,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s3. ')
 endif
  NULLIFY(grid%h2osoi_ice_s3)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16581,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s4. ')
 endif
  NULLIFY(grid%h2osoi_ice_s4)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16589,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s5. ')
 endif
  NULLIFY(grid%h2osoi_ice_s5)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16597,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice1. ')
 endif
  NULLIFY(grid%h2osoi_ice1)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16605,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice2. ')
 endif
  NULLIFY(grid%h2osoi_ice2)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16613,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice3. ')
 endif
  NULLIFY(grid%h2osoi_ice3)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16621,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice4. ')
 endif
  NULLIFY(grid%h2osoi_ice4)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16629,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice5. ')
 endif
  NULLIFY(grid%h2osoi_ice5)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16637,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice6. ')
 endif
  NULLIFY(grid%h2osoi_ice6)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16645,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice7. ')
 endif
  NULLIFY(grid%h2osoi_ice7)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16653,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice8. ')
 endif
  NULLIFY(grid%h2osoi_ice8)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16661,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice9. ')
 endif
  NULLIFY(grid%h2osoi_ice9)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16669,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice10. ')
 endif
  NULLIFY(grid%h2osoi_ice10)
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s1 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16677,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s1. ')
 endif
  NULLIFY(grid%t_soisno_s1)
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s2 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16685,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s2. ')
 endif
  NULLIFY(grid%t_soisno_s2)
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s3 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16693,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s3. ')
 endif
  NULLIFY(grid%t_soisno_s3)
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s4 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16701,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s4. ')
 endif
  NULLIFY(grid%t_soisno_s4)
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s5 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16709,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s5. ')
 endif
  NULLIFY(grid%t_soisno_s5)
ENDIF
IF ( ASSOCIATED( grid%t_soisno1 ) ) THEN 
  DEALLOCATE(grid%t_soisno1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16717,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno1. ')
 endif
  NULLIFY(grid%t_soisno1)
ENDIF
IF ( ASSOCIATED( grid%t_soisno2 ) ) THEN 
  DEALLOCATE(grid%t_soisno2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16725,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno2. ')
 endif
  NULLIFY(grid%t_soisno2)
ENDIF
IF ( ASSOCIATED( grid%t_soisno3 ) ) THEN 
  DEALLOCATE(grid%t_soisno3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16733,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno3. ')
 endif
  NULLIFY(grid%t_soisno3)
ENDIF
IF ( ASSOCIATED( grid%t_soisno4 ) ) THEN 
  DEALLOCATE(grid%t_soisno4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16741,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno4. ')
 endif
  NULLIFY(grid%t_soisno4)
ENDIF
IF ( ASSOCIATED( grid%t_soisno5 ) ) THEN 
  DEALLOCATE(grid%t_soisno5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16749,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno5. ')
 endif
  NULLIFY(grid%t_soisno5)
ENDIF
IF ( ASSOCIATED( grid%t_soisno6 ) ) THEN 
  DEALLOCATE(grid%t_soisno6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16757,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno6. ')
 endif
  NULLIFY(grid%t_soisno6)
ENDIF
IF ( ASSOCIATED( grid%t_soisno7 ) ) THEN 
  DEALLOCATE(grid%t_soisno7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16765,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno7. ')
 endif
  NULLIFY(grid%t_soisno7)
ENDIF
IF ( ASSOCIATED( grid%t_soisno8 ) ) THEN 
  DEALLOCATE(grid%t_soisno8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16773,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno8. ')
 endif
  NULLIFY(grid%t_soisno8)
ENDIF
IF ( ASSOCIATED( grid%t_soisno9 ) ) THEN 
  DEALLOCATE(grid%t_soisno9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16781,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno9. ')
 endif
  NULLIFY(grid%t_soisno9)
ENDIF
IF ( ASSOCIATED( grid%t_soisno10 ) ) THEN 
  DEALLOCATE(grid%t_soisno10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16789,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno10. ')
 endif
  NULLIFY(grid%t_soisno10)
ENDIF
IF ( ASSOCIATED( grid%dzsnow1 ) ) THEN 
  DEALLOCATE(grid%dzsnow1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16797,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow1. ')
 endif
  NULLIFY(grid%dzsnow1)
ENDIF
IF ( ASSOCIATED( grid%dzsnow2 ) ) THEN 
  DEALLOCATE(grid%dzsnow2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16805,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow2. ')
 endif
  NULLIFY(grid%dzsnow2)
ENDIF
IF ( ASSOCIATED( grid%dzsnow3 ) ) THEN 
  DEALLOCATE(grid%dzsnow3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16813,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow3. ')
 endif
  NULLIFY(grid%dzsnow3)
ENDIF
IF ( ASSOCIATED( grid%dzsnow4 ) ) THEN 
  DEALLOCATE(grid%dzsnow4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16821,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow4. ')
 endif
  NULLIFY(grid%dzsnow4)
ENDIF
IF ( ASSOCIATED( grid%dzsnow5 ) ) THEN 
  DEALLOCATE(grid%dzsnow5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16829,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow5. ')
 endif
  NULLIFY(grid%dzsnow5)
ENDIF
IF ( ASSOCIATED( grid%snowrds1 ) ) THEN 
  DEALLOCATE(grid%snowrds1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16837,&
'frame/module_domain.f: Failed to deallocate grid%snowrds1. ')
 endif
  NULLIFY(grid%snowrds1)
ENDIF
IF ( ASSOCIATED( grid%snowrds2 ) ) THEN 
  DEALLOCATE(grid%snowrds2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16845,&
'frame/module_domain.f: Failed to deallocate grid%snowrds2. ')
 endif
  NULLIFY(grid%snowrds2)
ENDIF
IF ( ASSOCIATED( grid%snowrds3 ) ) THEN 
  DEALLOCATE(grid%snowrds3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16853,&
'frame/module_domain.f: Failed to deallocate grid%snowrds3. ')
 endif
  NULLIFY(grid%snowrds3)
ENDIF
IF ( ASSOCIATED( grid%snowrds4 ) ) THEN 
  DEALLOCATE(grid%snowrds4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16861,&
'frame/module_domain.f: Failed to deallocate grid%snowrds4. ')
 endif
  NULLIFY(grid%snowrds4)
ENDIF
IF ( ASSOCIATED( grid%snowrds5 ) ) THEN 
  DEALLOCATE(grid%snowrds5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16869,&
'frame/module_domain.f: Failed to deallocate grid%snowrds5. ')
 endif
  NULLIFY(grid%snowrds5)
ENDIF
IF ( ASSOCIATED( grid%t_lake1 ) ) THEN 
  DEALLOCATE(grid%t_lake1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16877,&
'frame/module_domain.f: Failed to deallocate grid%t_lake1. ')
 endif
  NULLIFY(grid%t_lake1)
ENDIF
IF ( ASSOCIATED( grid%t_lake2 ) ) THEN 
  DEALLOCATE(grid%t_lake2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16885,&
'frame/module_domain.f: Failed to deallocate grid%t_lake2. ')
 endif
  NULLIFY(grid%t_lake2)
ENDIF
IF ( ASSOCIATED( grid%t_lake3 ) ) THEN 
  DEALLOCATE(grid%t_lake3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16893,&
'frame/module_domain.f: Failed to deallocate grid%t_lake3. ')
 endif
  NULLIFY(grid%t_lake3)
ENDIF
IF ( ASSOCIATED( grid%t_lake4 ) ) THEN 
  DEALLOCATE(grid%t_lake4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16901,&
'frame/module_domain.f: Failed to deallocate grid%t_lake4. ')
 endif
  NULLIFY(grid%t_lake4)
ENDIF
IF ( ASSOCIATED( grid%t_lake5 ) ) THEN 
  DEALLOCATE(grid%t_lake5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16909,&
'frame/module_domain.f: Failed to deallocate grid%t_lake5. ')
 endif
  NULLIFY(grid%t_lake5)
ENDIF
IF ( ASSOCIATED( grid%t_lake6 ) ) THEN 
  DEALLOCATE(grid%t_lake6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16917,&
'frame/module_domain.f: Failed to deallocate grid%t_lake6. ')
 endif
  NULLIFY(grid%t_lake6)
ENDIF
IF ( ASSOCIATED( grid%t_lake7 ) ) THEN 
  DEALLOCATE(grid%t_lake7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16925,&
'frame/module_domain.f: Failed to deallocate grid%t_lake7. ')
 endif
  NULLIFY(grid%t_lake7)
ENDIF
IF ( ASSOCIATED( grid%t_lake8 ) ) THEN 
  DEALLOCATE(grid%t_lake8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16933,&
'frame/module_domain.f: Failed to deallocate grid%t_lake8. ')
 endif
  NULLIFY(grid%t_lake8)
ENDIF
IF ( ASSOCIATED( grid%t_lake9 ) ) THEN 
  DEALLOCATE(grid%t_lake9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16941,&
'frame/module_domain.f: Failed to deallocate grid%t_lake9. ')
 endif
  NULLIFY(grid%t_lake9)
ENDIF
IF ( ASSOCIATED( grid%t_lake10 ) ) THEN 
  DEALLOCATE(grid%t_lake10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16949,&
'frame/module_domain.f: Failed to deallocate grid%t_lake10. ')
 endif
  NULLIFY(grid%t_lake10)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16957,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol1. ')
 endif
  NULLIFY(grid%h2osoi_vol1)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16965,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol2. ')
 endif
  NULLIFY(grid%h2osoi_vol2)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16973,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol3. ')
 endif
  NULLIFY(grid%h2osoi_vol3)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16981,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol4. ')
 endif
  NULLIFY(grid%h2osoi_vol4)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16989,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol5. ')
 endif
  NULLIFY(grid%h2osoi_vol5)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16997,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol6. ')
 endif
  NULLIFY(grid%h2osoi_vol6)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17005,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol7. ')
 endif
  NULLIFY(grid%h2osoi_vol7)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17013,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol8. ')
 endif
  NULLIFY(grid%h2osoi_vol8)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17021,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol9. ')
 endif
  NULLIFY(grid%h2osoi_vol9)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17029,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol10. ')
 endif
  NULLIFY(grid%h2osoi_vol10)
ENDIF
IF ( ASSOCIATED( grid%albedosubgrid ) ) THEN 
  DEALLOCATE(grid%albedosubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17037,&
'frame/module_domain.f: Failed to deallocate grid%albedosubgrid. ')
 endif
  NULLIFY(grid%albedosubgrid)
ENDIF
IF ( ASSOCIATED( grid%lhsubgrid ) ) THEN 
  DEALLOCATE(grid%lhsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17045,&
'frame/module_domain.f: Failed to deallocate grid%lhsubgrid. ')
 endif
  NULLIFY(grid%lhsubgrid)
ENDIF
IF ( ASSOCIATED( grid%hfxsubgrid ) ) THEN 
  DEALLOCATE(grid%hfxsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17053,&
'frame/module_domain.f: Failed to deallocate grid%hfxsubgrid. ')
 endif
  NULLIFY(grid%hfxsubgrid)
ENDIF
IF ( ASSOCIATED( grid%lwupsubgrid ) ) THEN 
  DEALLOCATE(grid%lwupsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17061,&
'frame/module_domain.f: Failed to deallocate grid%lwupsubgrid. ')
 endif
  NULLIFY(grid%lwupsubgrid)
ENDIF
IF ( ASSOCIATED( grid%q2subgrid ) ) THEN 
  DEALLOCATE(grid%q2subgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17069,&
'frame/module_domain.f: Failed to deallocate grid%q2subgrid. ')
 endif
  NULLIFY(grid%q2subgrid)
ENDIF
IF ( ASSOCIATED( grid%sabvsubgrid ) ) THEN 
  DEALLOCATE(grid%sabvsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17077,&
'frame/module_domain.f: Failed to deallocate grid%sabvsubgrid. ')
 endif
  NULLIFY(grid%sabvsubgrid)
ENDIF
IF ( ASSOCIATED( grid%sabgsubgrid ) ) THEN 
  DEALLOCATE(grid%sabgsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17085,&
'frame/module_domain.f: Failed to deallocate grid%sabgsubgrid. ')
 endif
  NULLIFY(grid%sabgsubgrid)
ENDIF
IF ( ASSOCIATED( grid%nrasubgrid ) ) THEN 
  DEALLOCATE(grid%nrasubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17093,&
'frame/module_domain.f: Failed to deallocate grid%nrasubgrid. ')
 endif
  NULLIFY(grid%nrasubgrid)
ENDIF
IF ( ASSOCIATED( grid%swupsubgrid ) ) THEN 
  DEALLOCATE(grid%swupsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17101,&
'frame/module_domain.f: Failed to deallocate grid%swupsubgrid. ')
 endif
  NULLIFY(grid%swupsubgrid)
ENDIF
IF ( ASSOCIATED( grid%lake2d ) ) THEN 
  DEALLOCATE(grid%lake2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17109,&
'frame/module_domain.f: Failed to deallocate grid%lake2d. ')
 endif
  NULLIFY(grid%lake2d)
ENDIF
IF ( ASSOCIATED( grid%lakedepth2d ) ) THEN 
  DEALLOCATE(grid%lakedepth2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17117,&
'frame/module_domain.f: Failed to deallocate grid%lakedepth2d. ')
 endif
  NULLIFY(grid%lakedepth2d)
ENDIF
IF ( ASSOCIATED( grid%savedtke12d ) ) THEN 
  DEALLOCATE(grid%savedtke12d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17125,&
'frame/module_domain.f: Failed to deallocate grid%savedtke12d. ')
 endif
  NULLIFY(grid%savedtke12d)
ENDIF
IF ( ASSOCIATED( grid%snowdp2d ) ) THEN 
  DEALLOCATE(grid%snowdp2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17133,&
'frame/module_domain.f: Failed to deallocate grid%snowdp2d. ')
 endif
  NULLIFY(grid%snowdp2d)
ENDIF
IF ( ASSOCIATED( grid%h2osno2d ) ) THEN 
  DEALLOCATE(grid%h2osno2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17141,&
'frame/module_domain.f: Failed to deallocate grid%h2osno2d. ')
 endif
  NULLIFY(grid%h2osno2d)
ENDIF
IF ( ASSOCIATED( grid%snl2d ) ) THEN 
  DEALLOCATE(grid%snl2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17149,&
'frame/module_domain.f: Failed to deallocate grid%snl2d. ')
 endif
  NULLIFY(grid%snl2d)
ENDIF
IF ( ASSOCIATED( grid%t_grnd2d ) ) THEN 
  DEALLOCATE(grid%t_grnd2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17157,&
'frame/module_domain.f: Failed to deallocate grid%t_grnd2d. ')
 endif
  NULLIFY(grid%t_grnd2d)
ENDIF
IF ( ASSOCIATED( grid%t_lake3d ) ) THEN 
  DEALLOCATE(grid%t_lake3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17165,&
'frame/module_domain.f: Failed to deallocate grid%t_lake3d. ')
 endif
  NULLIFY(grid%t_lake3d)
ENDIF
IF ( ASSOCIATED( grid%lake_icefrac3d ) ) THEN 
  DEALLOCATE(grid%lake_icefrac3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17173,&
'frame/module_domain.f: Failed to deallocate grid%lake_icefrac3d. ')
 endif
  NULLIFY(grid%lake_icefrac3d)
ENDIF
IF ( ASSOCIATED( grid%z_lake3d ) ) THEN 
  DEALLOCATE(grid%z_lake3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17181,&
'frame/module_domain.f: Failed to deallocate grid%z_lake3d. ')
 endif
  NULLIFY(grid%z_lake3d)
ENDIF
IF ( ASSOCIATED( grid%dz_lake3d ) ) THEN 
  DEALLOCATE(grid%dz_lake3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17189,&
'frame/module_domain.f: Failed to deallocate grid%dz_lake3d. ')
 endif
  NULLIFY(grid%dz_lake3d)
ENDIF
IF ( ASSOCIATED( grid%t_soisno3d ) ) THEN 
  DEALLOCATE(grid%t_soisno3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17197,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno3d. ')
 endif
  NULLIFY(grid%t_soisno3d)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17205,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice3d. ')
 endif
  NULLIFY(grid%h2osoi_ice3d)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17213,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq3d. ')
 endif
  NULLIFY(grid%h2osoi_liq3d)
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17221,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol3d. ')
 endif
  NULLIFY(grid%h2osoi_vol3d)
ENDIF
IF ( ASSOCIATED( grid%z3d ) ) THEN 
  DEALLOCATE(grid%z3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17229,&
'frame/module_domain.f: Failed to deallocate grid%z3d. ')
 endif
  NULLIFY(grid%z3d)
ENDIF
IF ( ASSOCIATED( grid%dz3d ) ) THEN 
  DEALLOCATE(grid%dz3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17237,&
'frame/module_domain.f: Failed to deallocate grid%dz3d. ')
 endif
  NULLIFY(grid%dz3d)
ENDIF
IF ( ASSOCIATED( grid%zi3d ) ) THEN 
  DEALLOCATE(grid%zi3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17245,&
'frame/module_domain.f: Failed to deallocate grid%zi3d. ')
 endif
  NULLIFY(grid%zi3d)
ENDIF
IF ( ASSOCIATED( grid%watsat3d ) ) THEN 
  DEALLOCATE(grid%watsat3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17253,&
'frame/module_domain.f: Failed to deallocate grid%watsat3d. ')
 endif
  NULLIFY(grid%watsat3d)
ENDIF
IF ( ASSOCIATED( grid%csol3d ) ) THEN 
  DEALLOCATE(grid%csol3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17261,&
'frame/module_domain.f: Failed to deallocate grid%csol3d. ')
 endif
  NULLIFY(grid%csol3d)
ENDIF
IF ( ASSOCIATED( grid%tkmg3d ) ) THEN 
  DEALLOCATE(grid%tkmg3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17269,&
'frame/module_domain.f: Failed to deallocate grid%tkmg3d. ')
 endif
  NULLIFY(grid%tkmg3d)
ENDIF
IF ( ASSOCIATED( grid%tkdry3d ) ) THEN 
  DEALLOCATE(grid%tkdry3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17277,&
'frame/module_domain.f: Failed to deallocate grid%tkdry3d. ')
 endif
  NULLIFY(grid%tkdry3d)
ENDIF
IF ( ASSOCIATED( grid%tksatu3d ) ) THEN 
  DEALLOCATE(grid%tksatu3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17285,&
'frame/module_domain.f: Failed to deallocate grid%tksatu3d. ')
 endif
  NULLIFY(grid%tksatu3d)
ENDIF
IF ( ASSOCIATED( grid%ssib_fm ) ) THEN 
  DEALLOCATE(grid%ssib_fm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17293,&
'frame/module_domain.f: Failed to deallocate grid%ssib_fm. ')
 endif
  NULLIFY(grid%ssib_fm)
ENDIF
IF ( ASSOCIATED( grid%ssib_fh ) ) THEN 
  DEALLOCATE(grid%ssib_fh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17301,&
'frame/module_domain.f: Failed to deallocate grid%ssib_fh. ')
 endif
  NULLIFY(grid%ssib_fh)
ENDIF
IF ( ASSOCIATED( grid%ssib_cm ) ) THEN 
  DEALLOCATE(grid%ssib_cm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17309,&
'frame/module_domain.f: Failed to deallocate grid%ssib_cm. ')
 endif
  NULLIFY(grid%ssib_cm)
ENDIF
IF ( ASSOCIATED( grid%ssibxdd ) ) THEN 
  DEALLOCATE(grid%ssibxdd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17317,&
'frame/module_domain.f: Failed to deallocate grid%ssibxdd. ')
 endif
  NULLIFY(grid%ssibxdd)
ENDIF
IF ( ASSOCIATED( grid%ssib_br ) ) THEN 
  DEALLOCATE(grid%ssib_br,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17325,&
'frame/module_domain.f: Failed to deallocate grid%ssib_br. ')
 endif
  NULLIFY(grid%ssib_br)
ENDIF
IF ( ASSOCIATED( grid%ssib_lhf ) ) THEN 
  DEALLOCATE(grid%ssib_lhf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17333,&
'frame/module_domain.f: Failed to deallocate grid%ssib_lhf. ')
 endif
  NULLIFY(grid%ssib_lhf)
ENDIF
IF ( ASSOCIATED( grid%ssib_shf ) ) THEN 
  DEALLOCATE(grid%ssib_shf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17341,&
'frame/module_domain.f: Failed to deallocate grid%ssib_shf. ')
 endif
  NULLIFY(grid%ssib_shf)
ENDIF
IF ( ASSOCIATED( grid%ssib_ghf ) ) THEN 
  DEALLOCATE(grid%ssib_ghf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17349,&
'frame/module_domain.f: Failed to deallocate grid%ssib_ghf. ')
 endif
  NULLIFY(grid%ssib_ghf)
ENDIF
IF ( ASSOCIATED( grid%ssib_egs ) ) THEN 
  DEALLOCATE(grid%ssib_egs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17357,&
'frame/module_domain.f: Failed to deallocate grid%ssib_egs. ')
 endif
  NULLIFY(grid%ssib_egs)
ENDIF
IF ( ASSOCIATED( grid%ssib_eci ) ) THEN 
  DEALLOCATE(grid%ssib_eci,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17365,&
'frame/module_domain.f: Failed to deallocate grid%ssib_eci. ')
 endif
  NULLIFY(grid%ssib_eci)
ENDIF
IF ( ASSOCIATED( grid%ssib_ect ) ) THEN 
  DEALLOCATE(grid%ssib_ect,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17373,&
'frame/module_domain.f: Failed to deallocate grid%ssib_ect. ')
 endif
  NULLIFY(grid%ssib_ect)
ENDIF
IF ( ASSOCIATED( grid%ssib_egi ) ) THEN 
  DEALLOCATE(grid%ssib_egi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17381,&
'frame/module_domain.f: Failed to deallocate grid%ssib_egi. ')
 endif
  NULLIFY(grid%ssib_egi)
ENDIF
IF ( ASSOCIATED( grid%ssib_egt ) ) THEN 
  DEALLOCATE(grid%ssib_egt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17389,&
'frame/module_domain.f: Failed to deallocate grid%ssib_egt. ')
 endif
  NULLIFY(grid%ssib_egt)
ENDIF
IF ( ASSOCIATED( grid%ssib_sdn ) ) THEN 
  DEALLOCATE(grid%ssib_sdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17397,&
'frame/module_domain.f: Failed to deallocate grid%ssib_sdn. ')
 endif
  NULLIFY(grid%ssib_sdn)
ENDIF
IF ( ASSOCIATED( grid%ssib_sup ) ) THEN 
  DEALLOCATE(grid%ssib_sup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17405,&
'frame/module_domain.f: Failed to deallocate grid%ssib_sup. ')
 endif
  NULLIFY(grid%ssib_sup)
ENDIF
IF ( ASSOCIATED( grid%ssib_ldn ) ) THEN 
  DEALLOCATE(grid%ssib_ldn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17413,&
'frame/module_domain.f: Failed to deallocate grid%ssib_ldn. ')
 endif
  NULLIFY(grid%ssib_ldn)
ENDIF
IF ( ASSOCIATED( grid%ssib_lup ) ) THEN 
  DEALLOCATE(grid%ssib_lup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17421,&
'frame/module_domain.f: Failed to deallocate grid%ssib_lup. ')
 endif
  NULLIFY(grid%ssib_lup)
ENDIF
IF ( ASSOCIATED( grid%ssib_wat ) ) THEN 
  DEALLOCATE(grid%ssib_wat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17429,&
'frame/module_domain.f: Failed to deallocate grid%ssib_wat. ')
 endif
  NULLIFY(grid%ssib_wat)
ENDIF
IF ( ASSOCIATED( grid%ssib_shc ) ) THEN 
  DEALLOCATE(grid%ssib_shc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17437,&
'frame/module_domain.f: Failed to deallocate grid%ssib_shc. ')
 endif
  NULLIFY(grid%ssib_shc)
ENDIF
IF ( ASSOCIATED( grid%ssib_shg ) ) THEN 
  DEALLOCATE(grid%ssib_shg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17445,&
'frame/module_domain.f: Failed to deallocate grid%ssib_shg. ')
 endif
  NULLIFY(grid%ssib_shg)
ENDIF
IF ( ASSOCIATED( grid%ssib_lai ) ) THEN 
  DEALLOCATE(grid%ssib_lai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17453,&
'frame/module_domain.f: Failed to deallocate grid%ssib_lai. ')
 endif
  NULLIFY(grid%ssib_lai)
ENDIF
IF ( ASSOCIATED( grid%ssib_vcf ) ) THEN 
  DEALLOCATE(grid%ssib_vcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17461,&
'frame/module_domain.f: Failed to deallocate grid%ssib_vcf. ')
 endif
  NULLIFY(grid%ssib_vcf)
ENDIF
IF ( ASSOCIATED( grid%ssib_z00 ) ) THEN 
  DEALLOCATE(grid%ssib_z00,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17469,&
'frame/module_domain.f: Failed to deallocate grid%ssib_z00. ')
 endif
  NULLIFY(grid%ssib_z00)
ENDIF
IF ( ASSOCIATED( grid%ssib_veg ) ) THEN 
  DEALLOCATE(grid%ssib_veg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17477,&
'frame/module_domain.f: Failed to deallocate grid%ssib_veg. ')
 endif
  NULLIFY(grid%ssib_veg)
ENDIF
IF ( ASSOCIATED( grid%isnow ) ) THEN 
  DEALLOCATE(grid%isnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17485,&
'frame/module_domain.f: Failed to deallocate grid%isnow. ')
 endif
  NULLIFY(grid%isnow)
ENDIF
IF ( ASSOCIATED( grid%swe ) ) THEN 
  DEALLOCATE(grid%swe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17493,&
'frame/module_domain.f: Failed to deallocate grid%swe. ')
 endif
  NULLIFY(grid%swe)
ENDIF
IF ( ASSOCIATED( grid%snowden ) ) THEN 
  DEALLOCATE(grid%snowden,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17501,&
'frame/module_domain.f: Failed to deallocate grid%snowden. ')
 endif
  NULLIFY(grid%snowden)
ENDIF
IF ( ASSOCIATED( grid%snowdepth ) ) THEN 
  DEALLOCATE(grid%snowdepth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17509,&
'frame/module_domain.f: Failed to deallocate grid%snowdepth. ')
 endif
  NULLIFY(grid%snowdepth)
ENDIF
IF ( ASSOCIATED( grid%tkair ) ) THEN 
  DEALLOCATE(grid%tkair,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17517,&
'frame/module_domain.f: Failed to deallocate grid%tkair. ')
 endif
  NULLIFY(grid%tkair)
ENDIF
IF ( ASSOCIATED( grid%dzo1 ) ) THEN 
  DEALLOCATE(grid%dzo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17525,&
'frame/module_domain.f: Failed to deallocate grid%dzo1. ')
 endif
  NULLIFY(grid%dzo1)
ENDIF
IF ( ASSOCIATED( grid%wo1 ) ) THEN 
  DEALLOCATE(grid%wo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17533,&
'frame/module_domain.f: Failed to deallocate grid%wo1. ')
 endif
  NULLIFY(grid%wo1)
ENDIF
IF ( ASSOCIATED( grid%tssn1 ) ) THEN 
  DEALLOCATE(grid%tssn1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17541,&
'frame/module_domain.f: Failed to deallocate grid%tssn1. ')
 endif
  NULLIFY(grid%tssn1)
ENDIF
IF ( ASSOCIATED( grid%tssno1 ) ) THEN 
  DEALLOCATE(grid%tssno1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17549,&
'frame/module_domain.f: Failed to deallocate grid%tssno1. ')
 endif
  NULLIFY(grid%tssno1)
ENDIF
IF ( ASSOCIATED( grid%bwo1 ) ) THEN 
  DEALLOCATE(grid%bwo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17557,&
'frame/module_domain.f: Failed to deallocate grid%bwo1. ')
 endif
  NULLIFY(grid%bwo1)
ENDIF
IF ( ASSOCIATED( grid%bto1 ) ) THEN 
  DEALLOCATE(grid%bto1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17565,&
'frame/module_domain.f: Failed to deallocate grid%bto1. ')
 endif
  NULLIFY(grid%bto1)
ENDIF
IF ( ASSOCIATED( grid%cto1 ) ) THEN 
  DEALLOCATE(grid%cto1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17573,&
'frame/module_domain.f: Failed to deallocate grid%cto1. ')
 endif
  NULLIFY(grid%cto1)
ENDIF
IF ( ASSOCIATED( grid%fio1 ) ) THEN 
  DEALLOCATE(grid%fio1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17581,&
'frame/module_domain.f: Failed to deallocate grid%fio1. ')
 endif
  NULLIFY(grid%fio1)
ENDIF
IF ( ASSOCIATED( grid%flo1 ) ) THEN 
  DEALLOCATE(grid%flo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17589,&
'frame/module_domain.f: Failed to deallocate grid%flo1. ')
 endif
  NULLIFY(grid%flo1)
ENDIF
IF ( ASSOCIATED( grid%bio1 ) ) THEN 
  DEALLOCATE(grid%bio1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17597,&
'frame/module_domain.f: Failed to deallocate grid%bio1. ')
 endif
  NULLIFY(grid%bio1)
ENDIF
IF ( ASSOCIATED( grid%blo1 ) ) THEN 
  DEALLOCATE(grid%blo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17605,&
'frame/module_domain.f: Failed to deallocate grid%blo1. ')
 endif
  NULLIFY(grid%blo1)
ENDIF
IF ( ASSOCIATED( grid%ho1 ) ) THEN 
  DEALLOCATE(grid%ho1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17613,&
'frame/module_domain.f: Failed to deallocate grid%ho1. ')
 endif
  NULLIFY(grid%ho1)
ENDIF
IF ( ASSOCIATED( grid%dzo2 ) ) THEN 
  DEALLOCATE(grid%dzo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17621,&
'frame/module_domain.f: Failed to deallocate grid%dzo2. ')
 endif
  NULLIFY(grid%dzo2)
ENDIF
IF ( ASSOCIATED( grid%wo2 ) ) THEN 
  DEALLOCATE(grid%wo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17629,&
'frame/module_domain.f: Failed to deallocate grid%wo2. ')
 endif
  NULLIFY(grid%wo2)
ENDIF
IF ( ASSOCIATED( grid%tssn2 ) ) THEN 
  DEALLOCATE(grid%tssn2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17637,&
'frame/module_domain.f: Failed to deallocate grid%tssn2. ')
 endif
  NULLIFY(grid%tssn2)
ENDIF
IF ( ASSOCIATED( grid%tssno2 ) ) THEN 
  DEALLOCATE(grid%tssno2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17645,&
'frame/module_domain.f: Failed to deallocate grid%tssno2. ')
 endif
  NULLIFY(grid%tssno2)
ENDIF
IF ( ASSOCIATED( grid%bwo2 ) ) THEN 
  DEALLOCATE(grid%bwo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17653,&
'frame/module_domain.f: Failed to deallocate grid%bwo2. ')
 endif
  NULLIFY(grid%bwo2)
ENDIF
IF ( ASSOCIATED( grid%bto2 ) ) THEN 
  DEALLOCATE(grid%bto2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17661,&
'frame/module_domain.f: Failed to deallocate grid%bto2. ')
 endif
  NULLIFY(grid%bto2)
ENDIF
IF ( ASSOCIATED( grid%cto2 ) ) THEN 
  DEALLOCATE(grid%cto2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17669,&
'frame/module_domain.f: Failed to deallocate grid%cto2. ')
 endif
  NULLIFY(grid%cto2)
ENDIF
IF ( ASSOCIATED( grid%fio2 ) ) THEN 
  DEALLOCATE(grid%fio2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17677,&
'frame/module_domain.f: Failed to deallocate grid%fio2. ')
 endif
  NULLIFY(grid%fio2)
ENDIF
IF ( ASSOCIATED( grid%flo2 ) ) THEN 
  DEALLOCATE(grid%flo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17685,&
'frame/module_domain.f: Failed to deallocate grid%flo2. ')
 endif
  NULLIFY(grid%flo2)
ENDIF
IF ( ASSOCIATED( grid%bio2 ) ) THEN 
  DEALLOCATE(grid%bio2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17693,&
'frame/module_domain.f: Failed to deallocate grid%bio2. ')
 endif
  NULLIFY(grid%bio2)
ENDIF
IF ( ASSOCIATED( grid%blo2 ) ) THEN 
  DEALLOCATE(grid%blo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17701,&
'frame/module_domain.f: Failed to deallocate grid%blo2. ')
 endif
  NULLIFY(grid%blo2)
ENDIF
IF ( ASSOCIATED( grid%ho2 ) ) THEN 
  DEALLOCATE(grid%ho2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17709,&
'frame/module_domain.f: Failed to deallocate grid%ho2. ')
 endif
  NULLIFY(grid%ho2)
ENDIF
IF ( ASSOCIATED( grid%dzo3 ) ) THEN 
  DEALLOCATE(grid%dzo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17717,&
'frame/module_domain.f: Failed to deallocate grid%dzo3. ')
 endif
  NULLIFY(grid%dzo3)
ENDIF
IF ( ASSOCIATED( grid%wo3 ) ) THEN 
  DEALLOCATE(grid%wo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17725,&
'frame/module_domain.f: Failed to deallocate grid%wo3. ')
 endif
  NULLIFY(grid%wo3)
ENDIF
IF ( ASSOCIATED( grid%tssn3 ) ) THEN 
  DEALLOCATE(grid%tssn3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17733,&
'frame/module_domain.f: Failed to deallocate grid%tssn3. ')
 endif
  NULLIFY(grid%tssn3)
ENDIF
IF ( ASSOCIATED( grid%tssno3 ) ) THEN 
  DEALLOCATE(grid%tssno3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17741,&
'frame/module_domain.f: Failed to deallocate grid%tssno3. ')
 endif
  NULLIFY(grid%tssno3)
ENDIF
IF ( ASSOCIATED( grid%bwo3 ) ) THEN 
  DEALLOCATE(grid%bwo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17749,&
'frame/module_domain.f: Failed to deallocate grid%bwo3. ')
 endif
  NULLIFY(grid%bwo3)
ENDIF
IF ( ASSOCIATED( grid%bto3 ) ) THEN 
  DEALLOCATE(grid%bto3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17757,&
'frame/module_domain.f: Failed to deallocate grid%bto3. ')
 endif
  NULLIFY(grid%bto3)
ENDIF
IF ( ASSOCIATED( grid%cto3 ) ) THEN 
  DEALLOCATE(grid%cto3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17765,&
'frame/module_domain.f: Failed to deallocate grid%cto3. ')
 endif
  NULLIFY(grid%cto3)
ENDIF
IF ( ASSOCIATED( grid%fio3 ) ) THEN 
  DEALLOCATE(grid%fio3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17773,&
'frame/module_domain.f: Failed to deallocate grid%fio3. ')
 endif
  NULLIFY(grid%fio3)
ENDIF
IF ( ASSOCIATED( grid%flo3 ) ) THEN 
  DEALLOCATE(grid%flo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17781,&
'frame/module_domain.f: Failed to deallocate grid%flo3. ')
 endif
  NULLIFY(grid%flo3)
ENDIF
IF ( ASSOCIATED( grid%bio3 ) ) THEN 
  DEALLOCATE(grid%bio3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17789,&
'frame/module_domain.f: Failed to deallocate grid%bio3. ')
 endif
  NULLIFY(grid%bio3)
ENDIF
IF ( ASSOCIATED( grid%blo3 ) ) THEN 
  DEALLOCATE(grid%blo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17797,&
'frame/module_domain.f: Failed to deallocate grid%blo3. ')
 endif
  NULLIFY(grid%blo3)
ENDIF
IF ( ASSOCIATED( grid%ho3 ) ) THEN 
  DEALLOCATE(grid%ho3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17805,&
'frame/module_domain.f: Failed to deallocate grid%ho3. ')
 endif
  NULLIFY(grid%ho3)
ENDIF
IF ( ASSOCIATED( grid%dzo4 ) ) THEN 
  DEALLOCATE(grid%dzo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17813,&
'frame/module_domain.f: Failed to deallocate grid%dzo4. ')
 endif
  NULLIFY(grid%dzo4)
ENDIF
IF ( ASSOCIATED( grid%wo4 ) ) THEN 
  DEALLOCATE(grid%wo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17821,&
'frame/module_domain.f: Failed to deallocate grid%wo4. ')
 endif
  NULLIFY(grid%wo4)
ENDIF
IF ( ASSOCIATED( grid%tssn4 ) ) THEN 
  DEALLOCATE(grid%tssn4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17829,&
'frame/module_domain.f: Failed to deallocate grid%tssn4. ')
 endif
  NULLIFY(grid%tssn4)
ENDIF
IF ( ASSOCIATED( grid%tssno4 ) ) THEN 
  DEALLOCATE(grid%tssno4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17837,&
'frame/module_domain.f: Failed to deallocate grid%tssno4. ')
 endif
  NULLIFY(grid%tssno4)
ENDIF
IF ( ASSOCIATED( grid%bwo4 ) ) THEN 
  DEALLOCATE(grid%bwo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17845,&
'frame/module_domain.f: Failed to deallocate grid%bwo4. ')
 endif
  NULLIFY(grid%bwo4)
ENDIF
IF ( ASSOCIATED( grid%bto4 ) ) THEN 
  DEALLOCATE(grid%bto4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17853,&
'frame/module_domain.f: Failed to deallocate grid%bto4. ')
 endif
  NULLIFY(grid%bto4)
ENDIF
IF ( ASSOCIATED( grid%cto4 ) ) THEN 
  DEALLOCATE(grid%cto4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17861,&
'frame/module_domain.f: Failed to deallocate grid%cto4. ')
 endif
  NULLIFY(grid%cto4)
ENDIF
IF ( ASSOCIATED( grid%fio4 ) ) THEN 
  DEALLOCATE(grid%fio4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17869,&
'frame/module_domain.f: Failed to deallocate grid%fio4. ')
 endif
  NULLIFY(grid%fio4)
ENDIF
IF ( ASSOCIATED( grid%flo4 ) ) THEN 
  DEALLOCATE(grid%flo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17877,&
'frame/module_domain.f: Failed to deallocate grid%flo4. ')
 endif
  NULLIFY(grid%flo4)
ENDIF
IF ( ASSOCIATED( grid%bio4 ) ) THEN 
  DEALLOCATE(grid%bio4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17885,&
'frame/module_domain.f: Failed to deallocate grid%bio4. ')
 endif
  NULLIFY(grid%bio4)
ENDIF
IF ( ASSOCIATED( grid%blo4 ) ) THEN 
  DEALLOCATE(grid%blo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17893,&
'frame/module_domain.f: Failed to deallocate grid%blo4. ')
 endif
  NULLIFY(grid%blo4)
ENDIF
IF ( ASSOCIATED( grid%ho4 ) ) THEN 
  DEALLOCATE(grid%ho4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17901,&
'frame/module_domain.f: Failed to deallocate grid%ho4. ')
 endif
  NULLIFY(grid%ho4)
ENDIF
IF ( ASSOCIATED( grid%isnowxy ) ) THEN 
  DEALLOCATE(grid%isnowxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17909,&
'frame/module_domain.f: Failed to deallocate grid%isnowxy. ')
 endif
  NULLIFY(grid%isnowxy)
ENDIF
IF ( ASSOCIATED( grid%tvxy ) ) THEN 
  DEALLOCATE(grid%tvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17917,&
'frame/module_domain.f: Failed to deallocate grid%tvxy. ')
 endif
  NULLIFY(grid%tvxy)
ENDIF
IF ( ASSOCIATED( grid%tgxy ) ) THEN 
  DEALLOCATE(grid%tgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17925,&
'frame/module_domain.f: Failed to deallocate grid%tgxy. ')
 endif
  NULLIFY(grid%tgxy)
ENDIF
IF ( ASSOCIATED( grid%canicexy ) ) THEN 
  DEALLOCATE(grid%canicexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17933,&
'frame/module_domain.f: Failed to deallocate grid%canicexy. ')
 endif
  NULLIFY(grid%canicexy)
ENDIF
IF ( ASSOCIATED( grid%canliqxy ) ) THEN 
  DEALLOCATE(grid%canliqxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17941,&
'frame/module_domain.f: Failed to deallocate grid%canliqxy. ')
 endif
  NULLIFY(grid%canliqxy)
ENDIF
IF ( ASSOCIATED( grid%eahxy ) ) THEN 
  DEALLOCATE(grid%eahxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17949,&
'frame/module_domain.f: Failed to deallocate grid%eahxy. ')
 endif
  NULLIFY(grid%eahxy)
ENDIF
IF ( ASSOCIATED( grid%tahxy ) ) THEN 
  DEALLOCATE(grid%tahxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17957,&
'frame/module_domain.f: Failed to deallocate grid%tahxy. ')
 endif
  NULLIFY(grid%tahxy)
ENDIF
IF ( ASSOCIATED( grid%cmxy ) ) THEN 
  DEALLOCATE(grid%cmxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17965,&
'frame/module_domain.f: Failed to deallocate grid%cmxy. ')
 endif
  NULLIFY(grid%cmxy)
ENDIF
IF ( ASSOCIATED( grid%chxy ) ) THEN 
  DEALLOCATE(grid%chxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17973,&
'frame/module_domain.f: Failed to deallocate grid%chxy. ')
 endif
  NULLIFY(grid%chxy)
ENDIF
IF ( ASSOCIATED( grid%fwetxy ) ) THEN 
  DEALLOCATE(grid%fwetxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17981,&
'frame/module_domain.f: Failed to deallocate grid%fwetxy. ')
 endif
  NULLIFY(grid%fwetxy)
ENDIF
IF ( ASSOCIATED( grid%sneqvoxy ) ) THEN 
  DEALLOCATE(grid%sneqvoxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17989,&
'frame/module_domain.f: Failed to deallocate grid%sneqvoxy. ')
 endif
  NULLIFY(grid%sneqvoxy)
ENDIF
IF ( ASSOCIATED( grid%alboldxy ) ) THEN 
  DEALLOCATE(grid%alboldxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",17997,&
'frame/module_domain.f: Failed to deallocate grid%alboldxy. ')
 endif
  NULLIFY(grid%alboldxy)
ENDIF
IF ( ASSOCIATED( grid%qsnowxy ) ) THEN 
  DEALLOCATE(grid%qsnowxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18005,&
'frame/module_domain.f: Failed to deallocate grid%qsnowxy. ')
 endif
  NULLIFY(grid%qsnowxy)
ENDIF
IF ( ASSOCIATED( grid%qrainxy ) ) THEN 
  DEALLOCATE(grid%qrainxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18013,&
'frame/module_domain.f: Failed to deallocate grid%qrainxy. ')
 endif
  NULLIFY(grid%qrainxy)
ENDIF
IF ( ASSOCIATED( grid%wslakexy ) ) THEN 
  DEALLOCATE(grid%wslakexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18021,&
'frame/module_domain.f: Failed to deallocate grid%wslakexy. ')
 endif
  NULLIFY(grid%wslakexy)
ENDIF
IF ( ASSOCIATED( grid%zwtxy ) ) THEN 
  DEALLOCATE(grid%zwtxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18029,&
'frame/module_domain.f: Failed to deallocate grid%zwtxy. ')
 endif
  NULLIFY(grid%zwtxy)
ENDIF
IF ( ASSOCIATED( grid%waxy ) ) THEN 
  DEALLOCATE(grid%waxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18037,&
'frame/module_domain.f: Failed to deallocate grid%waxy. ')
 endif
  NULLIFY(grid%waxy)
ENDIF
IF ( ASSOCIATED( grid%wtxy ) ) THEN 
  DEALLOCATE(grid%wtxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18045,&
'frame/module_domain.f: Failed to deallocate grid%wtxy. ')
 endif
  NULLIFY(grid%wtxy)
ENDIF
IF ( ASSOCIATED( grid%tsnoxy ) ) THEN 
  DEALLOCATE(grid%tsnoxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18053,&
'frame/module_domain.f: Failed to deallocate grid%tsnoxy. ')
 endif
  NULLIFY(grid%tsnoxy)
ENDIF
IF ( ASSOCIATED( grid%zsnsoxy ) ) THEN 
  DEALLOCATE(grid%zsnsoxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18061,&
'frame/module_domain.f: Failed to deallocate grid%zsnsoxy. ')
 endif
  NULLIFY(grid%zsnsoxy)
ENDIF
IF ( ASSOCIATED( grid%snicexy ) ) THEN 
  DEALLOCATE(grid%snicexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18069,&
'frame/module_domain.f: Failed to deallocate grid%snicexy. ')
 endif
  NULLIFY(grid%snicexy)
ENDIF
IF ( ASSOCIATED( grid%snliqxy ) ) THEN 
  DEALLOCATE(grid%snliqxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18077,&
'frame/module_domain.f: Failed to deallocate grid%snliqxy. ')
 endif
  NULLIFY(grid%snliqxy)
ENDIF
IF ( ASSOCIATED( grid%lfmassxy ) ) THEN 
  DEALLOCATE(grid%lfmassxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18085,&
'frame/module_domain.f: Failed to deallocate grid%lfmassxy. ')
 endif
  NULLIFY(grid%lfmassxy)
ENDIF
IF ( ASSOCIATED( grid%rtmassxy ) ) THEN 
  DEALLOCATE(grid%rtmassxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18093,&
'frame/module_domain.f: Failed to deallocate grid%rtmassxy. ')
 endif
  NULLIFY(grid%rtmassxy)
ENDIF
IF ( ASSOCIATED( grid%stmassxy ) ) THEN 
  DEALLOCATE(grid%stmassxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18101,&
'frame/module_domain.f: Failed to deallocate grid%stmassxy. ')
 endif
  NULLIFY(grid%stmassxy)
ENDIF
IF ( ASSOCIATED( grid%woodxy ) ) THEN 
  DEALLOCATE(grid%woodxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18109,&
'frame/module_domain.f: Failed to deallocate grid%woodxy. ')
 endif
  NULLIFY(grid%woodxy)
ENDIF
IF ( ASSOCIATED( grid%stblcpxy ) ) THEN 
  DEALLOCATE(grid%stblcpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18117,&
'frame/module_domain.f: Failed to deallocate grid%stblcpxy. ')
 endif
  NULLIFY(grid%stblcpxy)
ENDIF
IF ( ASSOCIATED( grid%fastcpxy ) ) THEN 
  DEALLOCATE(grid%fastcpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18125,&
'frame/module_domain.f: Failed to deallocate grid%fastcpxy. ')
 endif
  NULLIFY(grid%fastcpxy)
ENDIF
IF ( ASSOCIATED( grid%xsaixy ) ) THEN 
  DEALLOCATE(grid%xsaixy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18133,&
'frame/module_domain.f: Failed to deallocate grid%xsaixy. ')
 endif
  NULLIFY(grid%xsaixy)
ENDIF
IF ( ASSOCIATED( grid%taussxy ) ) THEN 
  DEALLOCATE(grid%taussxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18141,&
'frame/module_domain.f: Failed to deallocate grid%taussxy. ')
 endif
  NULLIFY(grid%taussxy)
ENDIF
IF ( ASSOCIATED( grid%t2mvxy ) ) THEN 
  DEALLOCATE(grid%t2mvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18149,&
'frame/module_domain.f: Failed to deallocate grid%t2mvxy. ')
 endif
  NULLIFY(grid%t2mvxy)
ENDIF
IF ( ASSOCIATED( grid%t2mbxy ) ) THEN 
  DEALLOCATE(grid%t2mbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18157,&
'frame/module_domain.f: Failed to deallocate grid%t2mbxy. ')
 endif
  NULLIFY(grid%t2mbxy)
ENDIF
IF ( ASSOCIATED( grid%q2mvxy ) ) THEN 
  DEALLOCATE(grid%q2mvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18165,&
'frame/module_domain.f: Failed to deallocate grid%q2mvxy. ')
 endif
  NULLIFY(grid%q2mvxy)
ENDIF
IF ( ASSOCIATED( grid%q2mbxy ) ) THEN 
  DEALLOCATE(grid%q2mbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18173,&
'frame/module_domain.f: Failed to deallocate grid%q2mbxy. ')
 endif
  NULLIFY(grid%q2mbxy)
ENDIF
IF ( ASSOCIATED( grid%tradxy ) ) THEN 
  DEALLOCATE(grid%tradxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18181,&
'frame/module_domain.f: Failed to deallocate grid%tradxy. ')
 endif
  NULLIFY(grid%tradxy)
ENDIF
IF ( ASSOCIATED( grid%neexy ) ) THEN 
  DEALLOCATE(grid%neexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18189,&
'frame/module_domain.f: Failed to deallocate grid%neexy. ')
 endif
  NULLIFY(grid%neexy)
ENDIF
IF ( ASSOCIATED( grid%gppxy ) ) THEN 
  DEALLOCATE(grid%gppxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18197,&
'frame/module_domain.f: Failed to deallocate grid%gppxy. ')
 endif
  NULLIFY(grid%gppxy)
ENDIF
IF ( ASSOCIATED( grid%nppxy ) ) THEN 
  DEALLOCATE(grid%nppxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18205,&
'frame/module_domain.f: Failed to deallocate grid%nppxy. ')
 endif
  NULLIFY(grid%nppxy)
ENDIF
IF ( ASSOCIATED( grid%fvegxy ) ) THEN 
  DEALLOCATE(grid%fvegxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18213,&
'frame/module_domain.f: Failed to deallocate grid%fvegxy. ')
 endif
  NULLIFY(grid%fvegxy)
ENDIF
IF ( ASSOCIATED( grid%qinxy ) ) THEN 
  DEALLOCATE(grid%qinxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18221,&
'frame/module_domain.f: Failed to deallocate grid%qinxy. ')
 endif
  NULLIFY(grid%qinxy)
ENDIF
IF ( ASSOCIATED( grid%runsfxy ) ) THEN 
  DEALLOCATE(grid%runsfxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18229,&
'frame/module_domain.f: Failed to deallocate grid%runsfxy. ')
 endif
  NULLIFY(grid%runsfxy)
ENDIF
IF ( ASSOCIATED( grid%runsbxy ) ) THEN 
  DEALLOCATE(grid%runsbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18237,&
'frame/module_domain.f: Failed to deallocate grid%runsbxy. ')
 endif
  NULLIFY(grid%runsbxy)
ENDIF
IF ( ASSOCIATED( grid%ecanxy ) ) THEN 
  DEALLOCATE(grid%ecanxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18245,&
'frame/module_domain.f: Failed to deallocate grid%ecanxy. ')
 endif
  NULLIFY(grid%ecanxy)
ENDIF
IF ( ASSOCIATED( grid%edirxy ) ) THEN 
  DEALLOCATE(grid%edirxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18253,&
'frame/module_domain.f: Failed to deallocate grid%edirxy. ')
 endif
  NULLIFY(grid%edirxy)
ENDIF
IF ( ASSOCIATED( grid%etranxy ) ) THEN 
  DEALLOCATE(grid%etranxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18261,&
'frame/module_domain.f: Failed to deallocate grid%etranxy. ')
 endif
  NULLIFY(grid%etranxy)
ENDIF
IF ( ASSOCIATED( grid%fsaxy ) ) THEN 
  DEALLOCATE(grid%fsaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18269,&
'frame/module_domain.f: Failed to deallocate grid%fsaxy. ')
 endif
  NULLIFY(grid%fsaxy)
ENDIF
IF ( ASSOCIATED( grid%firaxy ) ) THEN 
  DEALLOCATE(grid%firaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18277,&
'frame/module_domain.f: Failed to deallocate grid%firaxy. ')
 endif
  NULLIFY(grid%firaxy)
ENDIF
IF ( ASSOCIATED( grid%aparxy ) ) THEN 
  DEALLOCATE(grid%aparxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18285,&
'frame/module_domain.f: Failed to deallocate grid%aparxy. ')
 endif
  NULLIFY(grid%aparxy)
ENDIF
IF ( ASSOCIATED( grid%psnxy ) ) THEN 
  DEALLOCATE(grid%psnxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18293,&
'frame/module_domain.f: Failed to deallocate grid%psnxy. ')
 endif
  NULLIFY(grid%psnxy)
ENDIF
IF ( ASSOCIATED( grid%savxy ) ) THEN 
  DEALLOCATE(grid%savxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18301,&
'frame/module_domain.f: Failed to deallocate grid%savxy. ')
 endif
  NULLIFY(grid%savxy)
ENDIF
IF ( ASSOCIATED( grid%sagxy ) ) THEN 
  DEALLOCATE(grid%sagxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18309,&
'frame/module_domain.f: Failed to deallocate grid%sagxy. ')
 endif
  NULLIFY(grid%sagxy)
ENDIF
IF ( ASSOCIATED( grid%rssunxy ) ) THEN 
  DEALLOCATE(grid%rssunxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18317,&
'frame/module_domain.f: Failed to deallocate grid%rssunxy. ')
 endif
  NULLIFY(grid%rssunxy)
ENDIF
IF ( ASSOCIATED( grid%rsshaxy ) ) THEN 
  DEALLOCATE(grid%rsshaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18325,&
'frame/module_domain.f: Failed to deallocate grid%rsshaxy. ')
 endif
  NULLIFY(grid%rsshaxy)
ENDIF
IF ( ASSOCIATED( grid%bgapxy ) ) THEN 
  DEALLOCATE(grid%bgapxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18333,&
'frame/module_domain.f: Failed to deallocate grid%bgapxy. ')
 endif
  NULLIFY(grid%bgapxy)
ENDIF
IF ( ASSOCIATED( grid%wgapxy ) ) THEN 
  DEALLOCATE(grid%wgapxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18341,&
'frame/module_domain.f: Failed to deallocate grid%wgapxy. ')
 endif
  NULLIFY(grid%wgapxy)
ENDIF
IF ( ASSOCIATED( grid%tgvxy ) ) THEN 
  DEALLOCATE(grid%tgvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18349,&
'frame/module_domain.f: Failed to deallocate grid%tgvxy. ')
 endif
  NULLIFY(grid%tgvxy)
ENDIF
IF ( ASSOCIATED( grid%tgbxy ) ) THEN 
  DEALLOCATE(grid%tgbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18357,&
'frame/module_domain.f: Failed to deallocate grid%tgbxy. ')
 endif
  NULLIFY(grid%tgbxy)
ENDIF
IF ( ASSOCIATED( grid%chvxy ) ) THEN 
  DEALLOCATE(grid%chvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18365,&
'frame/module_domain.f: Failed to deallocate grid%chvxy. ')
 endif
  NULLIFY(grid%chvxy)
ENDIF
IF ( ASSOCIATED( grid%chbxy ) ) THEN 
  DEALLOCATE(grid%chbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18373,&
'frame/module_domain.f: Failed to deallocate grid%chbxy. ')
 endif
  NULLIFY(grid%chbxy)
ENDIF
IF ( ASSOCIATED( grid%shgxy ) ) THEN 
  DEALLOCATE(grid%shgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18381,&
'frame/module_domain.f: Failed to deallocate grid%shgxy. ')
 endif
  NULLIFY(grid%shgxy)
ENDIF
IF ( ASSOCIATED( grid%shcxy ) ) THEN 
  DEALLOCATE(grid%shcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18389,&
'frame/module_domain.f: Failed to deallocate grid%shcxy. ')
 endif
  NULLIFY(grid%shcxy)
ENDIF
IF ( ASSOCIATED( grid%shbxy ) ) THEN 
  DEALLOCATE(grid%shbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18397,&
'frame/module_domain.f: Failed to deallocate grid%shbxy. ')
 endif
  NULLIFY(grid%shbxy)
ENDIF
IF ( ASSOCIATED( grid%evgxy ) ) THEN 
  DEALLOCATE(grid%evgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18405,&
'frame/module_domain.f: Failed to deallocate grid%evgxy. ')
 endif
  NULLIFY(grid%evgxy)
ENDIF
IF ( ASSOCIATED( grid%evbxy ) ) THEN 
  DEALLOCATE(grid%evbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18413,&
'frame/module_domain.f: Failed to deallocate grid%evbxy. ')
 endif
  NULLIFY(grid%evbxy)
ENDIF
IF ( ASSOCIATED( grid%ghvxy ) ) THEN 
  DEALLOCATE(grid%ghvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18421,&
'frame/module_domain.f: Failed to deallocate grid%ghvxy. ')
 endif
  NULLIFY(grid%ghvxy)
ENDIF
IF ( ASSOCIATED( grid%ghbxy ) ) THEN 
  DEALLOCATE(grid%ghbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18429,&
'frame/module_domain.f: Failed to deallocate grid%ghbxy. ')
 endif
  NULLIFY(grid%ghbxy)
ENDIF
IF ( ASSOCIATED( grid%irgxy ) ) THEN 
  DEALLOCATE(grid%irgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18437,&
'frame/module_domain.f: Failed to deallocate grid%irgxy. ')
 endif
  NULLIFY(grid%irgxy)
ENDIF
IF ( ASSOCIATED( grid%ircxy ) ) THEN 
  DEALLOCATE(grid%ircxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18445,&
'frame/module_domain.f: Failed to deallocate grid%ircxy. ')
 endif
  NULLIFY(grid%ircxy)
ENDIF
IF ( ASSOCIATED( grid%irbxy ) ) THEN 
  DEALLOCATE(grid%irbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18453,&
'frame/module_domain.f: Failed to deallocate grid%irbxy. ')
 endif
  NULLIFY(grid%irbxy)
ENDIF
IF ( ASSOCIATED( grid%trxy ) ) THEN 
  DEALLOCATE(grid%trxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18461,&
'frame/module_domain.f: Failed to deallocate grid%trxy. ')
 endif
  NULLIFY(grid%trxy)
ENDIF
IF ( ASSOCIATED( grid%evcxy ) ) THEN 
  DEALLOCATE(grid%evcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18469,&
'frame/module_domain.f: Failed to deallocate grid%evcxy. ')
 endif
  NULLIFY(grid%evcxy)
ENDIF
IF ( ASSOCIATED( grid%chleafxy ) ) THEN 
  DEALLOCATE(grid%chleafxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18477,&
'frame/module_domain.f: Failed to deallocate grid%chleafxy. ')
 endif
  NULLIFY(grid%chleafxy)
ENDIF
IF ( ASSOCIATED( grid%chucxy ) ) THEN 
  DEALLOCATE(grid%chucxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18485,&
'frame/module_domain.f: Failed to deallocate grid%chucxy. ')
 endif
  NULLIFY(grid%chucxy)
ENDIF
IF ( ASSOCIATED( grid%chv2xy ) ) THEN 
  DEALLOCATE(grid%chv2xy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18493,&
'frame/module_domain.f: Failed to deallocate grid%chv2xy. ')
 endif
  NULLIFY(grid%chv2xy)
ENDIF
IF ( ASSOCIATED( grid%chb2xy ) ) THEN 
  DEALLOCATE(grid%chb2xy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18501,&
'frame/module_domain.f: Failed to deallocate grid%chb2xy. ')
 endif
  NULLIFY(grid%chb2xy)
ENDIF
IF ( ASSOCIATED( grid%chstarxy ) ) THEN 
  DEALLOCATE(grid%chstarxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18509,&
'frame/module_domain.f: Failed to deallocate grid%chstarxy. ')
 endif
  NULLIFY(grid%chstarxy)
ENDIF
IF ( ASSOCIATED( grid%smoiseq ) ) THEN 
  DEALLOCATE(grid%smoiseq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18517,&
'frame/module_domain.f: Failed to deallocate grid%smoiseq. ')
 endif
  NULLIFY(grid%smoiseq)
ENDIF
IF ( ASSOCIATED( grid%smcwtdxy ) ) THEN 
  DEALLOCATE(grid%smcwtdxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18525,&
'frame/module_domain.f: Failed to deallocate grid%smcwtdxy. ')
 endif
  NULLIFY(grid%smcwtdxy)
ENDIF
IF ( ASSOCIATED( grid%rechxy ) ) THEN 
  DEALLOCATE(grid%rechxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18533,&
'frame/module_domain.f: Failed to deallocate grid%rechxy. ')
 endif
  NULLIFY(grid%rechxy)
ENDIF
IF ( ASSOCIATED( grid%deeprechxy ) ) THEN 
  DEALLOCATE(grid%deeprechxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18541,&
'frame/module_domain.f: Failed to deallocate grid%deeprechxy. ')
 endif
  NULLIFY(grid%deeprechxy)
ENDIF
IF ( ASSOCIATED( grid%acrech ) ) THEN 
  DEALLOCATE(grid%acrech,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18549,&
'frame/module_domain.f: Failed to deallocate grid%acrech. ')
 endif
  NULLIFY(grid%acrech)
ENDIF
IF ( ASSOCIATED( grid%areaxy ) ) THEN 
  DEALLOCATE(grid%areaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18557,&
'frame/module_domain.f: Failed to deallocate grid%areaxy. ')
 endif
  NULLIFY(grid%areaxy)
ENDIF
IF ( ASSOCIATED( grid%qrfxy ) ) THEN 
  DEALLOCATE(grid%qrfxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18565,&
'frame/module_domain.f: Failed to deallocate grid%qrfxy. ')
 endif
  NULLIFY(grid%qrfxy)
ENDIF
IF ( ASSOCIATED( grid%qrfsxy ) ) THEN 
  DEALLOCATE(grid%qrfsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18573,&
'frame/module_domain.f: Failed to deallocate grid%qrfsxy. ')
 endif
  NULLIFY(grid%qrfsxy)
ENDIF
IF ( ASSOCIATED( grid%qspringxy ) ) THEN 
  DEALLOCATE(grid%qspringxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18581,&
'frame/module_domain.f: Failed to deallocate grid%qspringxy. ')
 endif
  NULLIFY(grid%qspringxy)
ENDIF
IF ( ASSOCIATED( grid%qspringsxy ) ) THEN 
  DEALLOCATE(grid%qspringsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18589,&
'frame/module_domain.f: Failed to deallocate grid%qspringsxy. ')
 endif
  NULLIFY(grid%qspringsxy)
ENDIF
IF ( ASSOCIATED( grid%acqspring ) ) THEN 
  DEALLOCATE(grid%acqspring,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18597,&
'frame/module_domain.f: Failed to deallocate grid%acqspring. ')
 endif
  NULLIFY(grid%acqspring)
ENDIF
IF ( ASSOCIATED( grid%qslatxy ) ) THEN 
  DEALLOCATE(grid%qslatxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18605,&
'frame/module_domain.f: Failed to deallocate grid%qslatxy. ')
 endif
  NULLIFY(grid%qslatxy)
ENDIF
IF ( ASSOCIATED( grid%qlatxy ) ) THEN 
  DEALLOCATE(grid%qlatxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18613,&
'frame/module_domain.f: Failed to deallocate grid%qlatxy. ')
 endif
  NULLIFY(grid%qlatxy)
ENDIF
IF ( ASSOCIATED( grid%pexpxy ) ) THEN 
  DEALLOCATE(grid%pexpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18621,&
'frame/module_domain.f: Failed to deallocate grid%pexpxy. ')
 endif
  NULLIFY(grid%pexpxy)
ENDIF
IF ( ASSOCIATED( grid%rivercondxy ) ) THEN 
  DEALLOCATE(grid%rivercondxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18629,&
'frame/module_domain.f: Failed to deallocate grid%rivercondxy. ')
 endif
  NULLIFY(grid%rivercondxy)
ENDIF
IF ( ASSOCIATED( grid%fdepthxy ) ) THEN 
  DEALLOCATE(grid%fdepthxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18637,&
'frame/module_domain.f: Failed to deallocate grid%fdepthxy. ')
 endif
  NULLIFY(grid%fdepthxy)
ENDIF
IF ( ASSOCIATED( grid%eqzwt ) ) THEN 
  DEALLOCATE(grid%eqzwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18645,&
'frame/module_domain.f: Failed to deallocate grid%eqzwt. ')
 endif
  NULLIFY(grid%eqzwt)
ENDIF
IF ( ASSOCIATED( grid%rechclim ) ) THEN 
  DEALLOCATE(grid%rechclim,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18653,&
'frame/module_domain.f: Failed to deallocate grid%rechclim. ')
 endif
  NULLIFY(grid%rechclim)
ENDIF
IF ( ASSOCIATED( grid%riverbedxy ) ) THEN 
  DEALLOCATE(grid%riverbedxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18661,&
'frame/module_domain.f: Failed to deallocate grid%riverbedxy. ')
 endif
  NULLIFY(grid%riverbedxy)
ENDIF
IF ( ASSOCIATED( grid%qintsxy ) ) THEN 
  DEALLOCATE(grid%qintsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18669,&
'frame/module_domain.f: Failed to deallocate grid%qintsxy. ')
 endif
  NULLIFY(grid%qintsxy)
ENDIF
IF ( ASSOCIATED( grid%qintrxy ) ) THEN 
  DEALLOCATE(grid%qintrxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18677,&
'frame/module_domain.f: Failed to deallocate grid%qintrxy. ')
 endif
  NULLIFY(grid%qintrxy)
ENDIF
IF ( ASSOCIATED( grid%qdripsxy ) ) THEN 
  DEALLOCATE(grid%qdripsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18685,&
'frame/module_domain.f: Failed to deallocate grid%qdripsxy. ')
 endif
  NULLIFY(grid%qdripsxy)
ENDIF
IF ( ASSOCIATED( grid%qdriprxy ) ) THEN 
  DEALLOCATE(grid%qdriprxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18693,&
'frame/module_domain.f: Failed to deallocate grid%qdriprxy. ')
 endif
  NULLIFY(grid%qdriprxy)
ENDIF
IF ( ASSOCIATED( grid%qthrosxy ) ) THEN 
  DEALLOCATE(grid%qthrosxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18701,&
'frame/module_domain.f: Failed to deallocate grid%qthrosxy. ')
 endif
  NULLIFY(grid%qthrosxy)
ENDIF
IF ( ASSOCIATED( grid%qthrorxy ) ) THEN 
  DEALLOCATE(grid%qthrorxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18709,&
'frame/module_domain.f: Failed to deallocate grid%qthrorxy. ')
 endif
  NULLIFY(grid%qthrorxy)
ENDIF
IF ( ASSOCIATED( grid%qsnsubxy ) ) THEN 
  DEALLOCATE(grid%qsnsubxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18717,&
'frame/module_domain.f: Failed to deallocate grid%qsnsubxy. ')
 endif
  NULLIFY(grid%qsnsubxy)
ENDIF
IF ( ASSOCIATED( grid%qsnfroxy ) ) THEN 
  DEALLOCATE(grid%qsnfroxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18725,&
'frame/module_domain.f: Failed to deallocate grid%qsnfroxy. ')
 endif
  NULLIFY(grid%qsnfroxy)
ENDIF
IF ( ASSOCIATED( grid%qsubcxy ) ) THEN 
  DEALLOCATE(grid%qsubcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18733,&
'frame/module_domain.f: Failed to deallocate grid%qsubcxy. ')
 endif
  NULLIFY(grid%qsubcxy)
ENDIF
IF ( ASSOCIATED( grid%qfrocxy ) ) THEN 
  DEALLOCATE(grid%qfrocxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18741,&
'frame/module_domain.f: Failed to deallocate grid%qfrocxy. ')
 endif
  NULLIFY(grid%qfrocxy)
ENDIF
IF ( ASSOCIATED( grid%qevacxy ) ) THEN 
  DEALLOCATE(grid%qevacxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18749,&
'frame/module_domain.f: Failed to deallocate grid%qevacxy. ')
 endif
  NULLIFY(grid%qevacxy)
ENDIF
IF ( ASSOCIATED( grid%qdewcxy ) ) THEN 
  DEALLOCATE(grid%qdewcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18757,&
'frame/module_domain.f: Failed to deallocate grid%qdewcxy. ')
 endif
  NULLIFY(grid%qdewcxy)
ENDIF
IF ( ASSOCIATED( grid%qfrzcxy ) ) THEN 
  DEALLOCATE(grid%qfrzcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18765,&
'frame/module_domain.f: Failed to deallocate grid%qfrzcxy. ')
 endif
  NULLIFY(grid%qfrzcxy)
ENDIF
IF ( ASSOCIATED( grid%qmeltcxy ) ) THEN 
  DEALLOCATE(grid%qmeltcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18773,&
'frame/module_domain.f: Failed to deallocate grid%qmeltcxy. ')
 endif
  NULLIFY(grid%qmeltcxy)
ENDIF
IF ( ASSOCIATED( grid%qsnbotxy ) ) THEN 
  DEALLOCATE(grid%qsnbotxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18781,&
'frame/module_domain.f: Failed to deallocate grid%qsnbotxy. ')
 endif
  NULLIFY(grid%qsnbotxy)
ENDIF
IF ( ASSOCIATED( grid%qmeltxy ) ) THEN 
  DEALLOCATE(grid%qmeltxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18789,&
'frame/module_domain.f: Failed to deallocate grid%qmeltxy. ')
 endif
  NULLIFY(grid%qmeltxy)
ENDIF
IF ( ASSOCIATED( grid%pondingxy ) ) THEN 
  DEALLOCATE(grid%pondingxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18797,&
'frame/module_domain.f: Failed to deallocate grid%pondingxy. ')
 endif
  NULLIFY(grid%pondingxy)
ENDIF
IF ( ASSOCIATED( grid%pahxy ) ) THEN 
  DEALLOCATE(grid%pahxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18805,&
'frame/module_domain.f: Failed to deallocate grid%pahxy. ')
 endif
  NULLIFY(grid%pahxy)
ENDIF
IF ( ASSOCIATED( grid%pahgxy ) ) THEN 
  DEALLOCATE(grid%pahgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18813,&
'frame/module_domain.f: Failed to deallocate grid%pahgxy. ')
 endif
  NULLIFY(grid%pahgxy)
ENDIF
IF ( ASSOCIATED( grid%pahvxy ) ) THEN 
  DEALLOCATE(grid%pahvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18821,&
'frame/module_domain.f: Failed to deallocate grid%pahvxy. ')
 endif
  NULLIFY(grid%pahvxy)
ENDIF
IF ( ASSOCIATED( grid%pahbxy ) ) THEN 
  DEALLOCATE(grid%pahbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18829,&
'frame/module_domain.f: Failed to deallocate grid%pahbxy. ')
 endif
  NULLIFY(grid%pahbxy)
ENDIF
IF ( ASSOCIATED( grid%canhsxy ) ) THEN 
  DEALLOCATE(grid%canhsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18837,&
'frame/module_domain.f: Failed to deallocate grid%canhsxy. ')
 endif
  NULLIFY(grid%canhsxy)
ENDIF
IF ( ASSOCIATED( grid%fpicexy ) ) THEN 
  DEALLOCATE(grid%fpicexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18845,&
'frame/module_domain.f: Failed to deallocate grid%fpicexy. ')
 endif
  NULLIFY(grid%fpicexy)
ENDIF
IF ( ASSOCIATED( grid%rainlsm ) ) THEN 
  DEALLOCATE(grid%rainlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18853,&
'frame/module_domain.f: Failed to deallocate grid%rainlsm. ')
 endif
  NULLIFY(grid%rainlsm)
ENDIF
IF ( ASSOCIATED( grid%snowlsm ) ) THEN 
  DEALLOCATE(grid%snowlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18861,&
'frame/module_domain.f: Failed to deallocate grid%snowlsm. ')
 endif
  NULLIFY(grid%snowlsm)
ENDIF
IF ( ASSOCIATED( grid%soilcomp ) ) THEN 
  DEALLOCATE(grid%soilcomp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18869,&
'frame/module_domain.f: Failed to deallocate grid%soilcomp. ')
 endif
  NULLIFY(grid%soilcomp)
ENDIF
IF ( ASSOCIATED( grid%soilcl1 ) ) THEN 
  DEALLOCATE(grid%soilcl1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18877,&
'frame/module_domain.f: Failed to deallocate grid%soilcl1. ')
 endif
  NULLIFY(grid%soilcl1)
ENDIF
IF ( ASSOCIATED( grid%soilcl2 ) ) THEN 
  DEALLOCATE(grid%soilcl2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18885,&
'frame/module_domain.f: Failed to deallocate grid%soilcl2. ')
 endif
  NULLIFY(grid%soilcl2)
ENDIF
IF ( ASSOCIATED( grid%soilcl3 ) ) THEN 
  DEALLOCATE(grid%soilcl3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18893,&
'frame/module_domain.f: Failed to deallocate grid%soilcl3. ')
 endif
  NULLIFY(grid%soilcl3)
ENDIF
IF ( ASSOCIATED( grid%soilcl4 ) ) THEN 
  DEALLOCATE(grid%soilcl4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18901,&
'frame/module_domain.f: Failed to deallocate grid%soilcl4. ')
 endif
  NULLIFY(grid%soilcl4)
ENDIF
IF ( ASSOCIATED( grid%acints ) ) THEN 
  DEALLOCATE(grid%acints,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18909,&
'frame/module_domain.f: Failed to deallocate grid%acints. ')
 endif
  NULLIFY(grid%acints)
ENDIF
IF ( ASSOCIATED( grid%acintr ) ) THEN 
  DEALLOCATE(grid%acintr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18917,&
'frame/module_domain.f: Failed to deallocate grid%acintr. ')
 endif
  NULLIFY(grid%acintr)
ENDIF
IF ( ASSOCIATED( grid%acdripr ) ) THEN 
  DEALLOCATE(grid%acdripr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18925,&
'frame/module_domain.f: Failed to deallocate grid%acdripr. ')
 endif
  NULLIFY(grid%acdripr)
ENDIF
IF ( ASSOCIATED( grid%acthror ) ) THEN 
  DEALLOCATE(grid%acthror,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18933,&
'frame/module_domain.f: Failed to deallocate grid%acthror. ')
 endif
  NULLIFY(grid%acthror)
ENDIF
IF ( ASSOCIATED( grid%acevac ) ) THEN 
  DEALLOCATE(grid%acevac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18941,&
'frame/module_domain.f: Failed to deallocate grid%acevac. ')
 endif
  NULLIFY(grid%acevac)
ENDIF
IF ( ASSOCIATED( grid%acdewc ) ) THEN 
  DEALLOCATE(grid%acdewc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18949,&
'frame/module_domain.f: Failed to deallocate grid%acdewc. ')
 endif
  NULLIFY(grid%acdewc)
ENDIF
IF ( ASSOCIATED( grid%forctlsm ) ) THEN 
  DEALLOCATE(grid%forctlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18957,&
'frame/module_domain.f: Failed to deallocate grid%forctlsm. ')
 endif
  NULLIFY(grid%forctlsm)
ENDIF
IF ( ASSOCIATED( grid%forcqlsm ) ) THEN 
  DEALLOCATE(grid%forcqlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18965,&
'frame/module_domain.f: Failed to deallocate grid%forcqlsm. ')
 endif
  NULLIFY(grid%forcqlsm)
ENDIF
IF ( ASSOCIATED( grid%forcplsm ) ) THEN 
  DEALLOCATE(grid%forcplsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18973,&
'frame/module_domain.f: Failed to deallocate grid%forcplsm. ')
 endif
  NULLIFY(grid%forcplsm)
ENDIF
IF ( ASSOCIATED( grid%forczlsm ) ) THEN 
  DEALLOCATE(grid%forczlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18981,&
'frame/module_domain.f: Failed to deallocate grid%forczlsm. ')
 endif
  NULLIFY(grid%forczlsm)
ENDIF
IF ( ASSOCIATED( grid%forcwlsm ) ) THEN 
  DEALLOCATE(grid%forcwlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18989,&
'frame/module_domain.f: Failed to deallocate grid%forcwlsm. ')
 endif
  NULLIFY(grid%forcwlsm)
ENDIF
IF ( ASSOCIATED( grid%acrainlsm ) ) THEN 
  DEALLOCATE(grid%acrainlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",18997,&
'frame/module_domain.f: Failed to deallocate grid%acrainlsm. ')
 endif
  NULLIFY(grid%acrainlsm)
ENDIF
IF ( ASSOCIATED( grid%acrunsb ) ) THEN 
  DEALLOCATE(grid%acrunsb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19005,&
'frame/module_domain.f: Failed to deallocate grid%acrunsb. ')
 endif
  NULLIFY(grid%acrunsb)
ENDIF
IF ( ASSOCIATED( grid%acrunsf ) ) THEN 
  DEALLOCATE(grid%acrunsf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19013,&
'frame/module_domain.f: Failed to deallocate grid%acrunsf. ')
 endif
  NULLIFY(grid%acrunsf)
ENDIF
IF ( ASSOCIATED( grid%acecan ) ) THEN 
  DEALLOCATE(grid%acecan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19021,&
'frame/module_domain.f: Failed to deallocate grid%acecan. ')
 endif
  NULLIFY(grid%acecan)
ENDIF
IF ( ASSOCIATED( grid%acetran ) ) THEN 
  DEALLOCATE(grid%acetran,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19029,&
'frame/module_domain.f: Failed to deallocate grid%acetran. ')
 endif
  NULLIFY(grid%acetran)
ENDIF
IF ( ASSOCIATED( grid%acedir ) ) THEN 
  DEALLOCATE(grid%acedir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19037,&
'frame/module_domain.f: Failed to deallocate grid%acedir. ')
 endif
  NULLIFY(grid%acedir)
ENDIF
IF ( ASSOCIATED( grid%acqlat ) ) THEN 
  DEALLOCATE(grid%acqlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19045,&
'frame/module_domain.f: Failed to deallocate grid%acqlat. ')
 endif
  NULLIFY(grid%acqlat)
ENDIF
IF ( ASSOCIATED( grid%acqrf ) ) THEN 
  DEALLOCATE(grid%acqrf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19053,&
'frame/module_domain.f: Failed to deallocate grid%acqrf. ')
 endif
  NULLIFY(grid%acqrf)
ENDIF
IF ( ASSOCIATED( grid%acetlsm ) ) THEN 
  DEALLOCATE(grid%acetlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19061,&
'frame/module_domain.f: Failed to deallocate grid%acetlsm. ')
 endif
  NULLIFY(grid%acetlsm)
ENDIF
IF ( ASSOCIATED( grid%acsnowlsm ) ) THEN 
  DEALLOCATE(grid%acsnowlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19069,&
'frame/module_domain.f: Failed to deallocate grid%acsnowlsm. ')
 endif
  NULLIFY(grid%acsnowlsm)
ENDIF
IF ( ASSOCIATED( grid%acsubc ) ) THEN 
  DEALLOCATE(grid%acsubc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19077,&
'frame/module_domain.f: Failed to deallocate grid%acsubc. ')
 endif
  NULLIFY(grid%acsubc)
ENDIF
IF ( ASSOCIATED( grid%acfroc ) ) THEN 
  DEALLOCATE(grid%acfroc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19085,&
'frame/module_domain.f: Failed to deallocate grid%acfroc. ')
 endif
  NULLIFY(grid%acfroc)
ENDIF
IF ( ASSOCIATED( grid%acfrzc ) ) THEN 
  DEALLOCATE(grid%acfrzc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19093,&
'frame/module_domain.f: Failed to deallocate grid%acfrzc. ')
 endif
  NULLIFY(grid%acfrzc)
ENDIF
IF ( ASSOCIATED( grid%acmeltc ) ) THEN 
  DEALLOCATE(grid%acmeltc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19101,&
'frame/module_domain.f: Failed to deallocate grid%acmeltc. ')
 endif
  NULLIFY(grid%acmeltc)
ENDIF
IF ( ASSOCIATED( grid%acsnbot ) ) THEN 
  DEALLOCATE(grid%acsnbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19109,&
'frame/module_domain.f: Failed to deallocate grid%acsnbot. ')
 endif
  NULLIFY(grid%acsnbot)
ENDIF
IF ( ASSOCIATED( grid%acsnmelt ) ) THEN 
  DEALLOCATE(grid%acsnmelt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19117,&
'frame/module_domain.f: Failed to deallocate grid%acsnmelt. ')
 endif
  NULLIFY(grid%acsnmelt)
ENDIF
IF ( ASSOCIATED( grid%acponding ) ) THEN 
  DEALLOCATE(grid%acponding,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19125,&
'frame/module_domain.f: Failed to deallocate grid%acponding. ')
 endif
  NULLIFY(grid%acponding)
ENDIF
IF ( ASSOCIATED( grid%acsnsub ) ) THEN 
  DEALLOCATE(grid%acsnsub,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19133,&
'frame/module_domain.f: Failed to deallocate grid%acsnsub. ')
 endif
  NULLIFY(grid%acsnsub)
ENDIF
IF ( ASSOCIATED( grid%acsnfro ) ) THEN 
  DEALLOCATE(grid%acsnfro,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19141,&
'frame/module_domain.f: Failed to deallocate grid%acsnfro. ')
 endif
  NULLIFY(grid%acsnfro)
ENDIF
IF ( ASSOCIATED( grid%acrainsnow ) ) THEN 
  DEALLOCATE(grid%acrainsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19149,&
'frame/module_domain.f: Failed to deallocate grid%acrainsnow. ')
 endif
  NULLIFY(grid%acrainsnow)
ENDIF
IF ( ASSOCIATED( grid%acdrips ) ) THEN 
  DEALLOCATE(grid%acdrips,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19157,&
'frame/module_domain.f: Failed to deallocate grid%acdrips. ')
 endif
  NULLIFY(grid%acdrips)
ENDIF
IF ( ASSOCIATED( grid%acthros ) ) THEN 
  DEALLOCATE(grid%acthros,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19165,&
'frame/module_domain.f: Failed to deallocate grid%acthros. ')
 endif
  NULLIFY(grid%acthros)
ENDIF
IF ( ASSOCIATED( grid%acsagb ) ) THEN 
  DEALLOCATE(grid%acsagb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19173,&
'frame/module_domain.f: Failed to deallocate grid%acsagb. ')
 endif
  NULLIFY(grid%acsagb)
ENDIF
IF ( ASSOCIATED( grid%acirb ) ) THEN 
  DEALLOCATE(grid%acirb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19181,&
'frame/module_domain.f: Failed to deallocate grid%acirb. ')
 endif
  NULLIFY(grid%acirb)
ENDIF
IF ( ASSOCIATED( grid%acshb ) ) THEN 
  DEALLOCATE(grid%acshb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19189,&
'frame/module_domain.f: Failed to deallocate grid%acshb. ')
 endif
  NULLIFY(grid%acshb)
ENDIF
IF ( ASSOCIATED( grid%acevb ) ) THEN 
  DEALLOCATE(grid%acevb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19197,&
'frame/module_domain.f: Failed to deallocate grid%acevb. ')
 endif
  NULLIFY(grid%acevb)
ENDIF
IF ( ASSOCIATED( grid%acghb ) ) THEN 
  DEALLOCATE(grid%acghb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19205,&
'frame/module_domain.f: Failed to deallocate grid%acghb. ')
 endif
  NULLIFY(grid%acghb)
ENDIF
IF ( ASSOCIATED( grid%acpahb ) ) THEN 
  DEALLOCATE(grid%acpahb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19213,&
'frame/module_domain.f: Failed to deallocate grid%acpahb. ')
 endif
  NULLIFY(grid%acpahb)
ENDIF
IF ( ASSOCIATED( grid%acsagv ) ) THEN 
  DEALLOCATE(grid%acsagv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19221,&
'frame/module_domain.f: Failed to deallocate grid%acsagv. ')
 endif
  NULLIFY(grid%acsagv)
ENDIF
IF ( ASSOCIATED( grid%acirg ) ) THEN 
  DEALLOCATE(grid%acirg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19229,&
'frame/module_domain.f: Failed to deallocate grid%acirg. ')
 endif
  NULLIFY(grid%acirg)
ENDIF
IF ( ASSOCIATED( grid%acshg ) ) THEN 
  DEALLOCATE(grid%acshg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19237,&
'frame/module_domain.f: Failed to deallocate grid%acshg. ')
 endif
  NULLIFY(grid%acshg)
ENDIF
IF ( ASSOCIATED( grid%acevg ) ) THEN 
  DEALLOCATE(grid%acevg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19245,&
'frame/module_domain.f: Failed to deallocate grid%acevg. ')
 endif
  NULLIFY(grid%acevg)
ENDIF
IF ( ASSOCIATED( grid%acghv ) ) THEN 
  DEALLOCATE(grid%acghv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19253,&
'frame/module_domain.f: Failed to deallocate grid%acghv. ')
 endif
  NULLIFY(grid%acghv)
ENDIF
IF ( ASSOCIATED( grid%acpahg ) ) THEN 
  DEALLOCATE(grid%acpahg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19261,&
'frame/module_domain.f: Failed to deallocate grid%acpahg. ')
 endif
  NULLIFY(grid%acpahg)
ENDIF
IF ( ASSOCIATED( grid%acsav ) ) THEN 
  DEALLOCATE(grid%acsav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19269,&
'frame/module_domain.f: Failed to deallocate grid%acsav. ')
 endif
  NULLIFY(grid%acsav)
ENDIF
IF ( ASSOCIATED( grid%acirc ) ) THEN 
  DEALLOCATE(grid%acirc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19277,&
'frame/module_domain.f: Failed to deallocate grid%acirc. ')
 endif
  NULLIFY(grid%acirc)
ENDIF
IF ( ASSOCIATED( grid%acshc ) ) THEN 
  DEALLOCATE(grid%acshc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19285,&
'frame/module_domain.f: Failed to deallocate grid%acshc. ')
 endif
  NULLIFY(grid%acshc)
ENDIF
IF ( ASSOCIATED( grid%acevc ) ) THEN 
  DEALLOCATE(grid%acevc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19293,&
'frame/module_domain.f: Failed to deallocate grid%acevc. ')
 endif
  NULLIFY(grid%acevc)
ENDIF
IF ( ASSOCIATED( grid%actr ) ) THEN 
  DEALLOCATE(grid%actr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19301,&
'frame/module_domain.f: Failed to deallocate grid%actr. ')
 endif
  NULLIFY(grid%actr)
ENDIF
IF ( ASSOCIATED( grid%acpahv ) ) THEN 
  DEALLOCATE(grid%acpahv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19309,&
'frame/module_domain.f: Failed to deallocate grid%acpahv. ')
 endif
  NULLIFY(grid%acpahv)
ENDIF
IF ( ASSOCIATED( grid%acswdnlsm ) ) THEN 
  DEALLOCATE(grid%acswdnlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19317,&
'frame/module_domain.f: Failed to deallocate grid%acswdnlsm. ')
 endif
  NULLIFY(grid%acswdnlsm)
ENDIF
IF ( ASSOCIATED( grid%acswuplsm ) ) THEN 
  DEALLOCATE(grid%acswuplsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19325,&
'frame/module_domain.f: Failed to deallocate grid%acswuplsm. ')
 endif
  NULLIFY(grid%acswuplsm)
ENDIF
IF ( ASSOCIATED( grid%aclwdnlsm ) ) THEN 
  DEALLOCATE(grid%aclwdnlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19333,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnlsm. ')
 endif
  NULLIFY(grid%aclwdnlsm)
ENDIF
IF ( ASSOCIATED( grid%aclwuplsm ) ) THEN 
  DEALLOCATE(grid%aclwuplsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19341,&
'frame/module_domain.f: Failed to deallocate grid%aclwuplsm. ')
 endif
  NULLIFY(grid%aclwuplsm)
ENDIF
IF ( ASSOCIATED( grid%acshflsm ) ) THEN 
  DEALLOCATE(grid%acshflsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19349,&
'frame/module_domain.f: Failed to deallocate grid%acshflsm. ')
 endif
  NULLIFY(grid%acshflsm)
ENDIF
IF ( ASSOCIATED( grid%aclhflsm ) ) THEN 
  DEALLOCATE(grid%aclhflsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19357,&
'frame/module_domain.f: Failed to deallocate grid%aclhflsm. ')
 endif
  NULLIFY(grid%aclhflsm)
ENDIF
IF ( ASSOCIATED( grid%acghflsm ) ) THEN 
  DEALLOCATE(grid%acghflsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19365,&
'frame/module_domain.f: Failed to deallocate grid%acghflsm. ')
 endif
  NULLIFY(grid%acghflsm)
ENDIF
IF ( ASSOCIATED( grid%acpahlsm ) ) THEN 
  DEALLOCATE(grid%acpahlsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19373,&
'frame/module_domain.f: Failed to deallocate grid%acpahlsm. ')
 endif
  NULLIFY(grid%acpahlsm)
ENDIF
IF ( ASSOCIATED( grid%accanhs ) ) THEN 
  DEALLOCATE(grid%accanhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19381,&
'frame/module_domain.f: Failed to deallocate grid%accanhs. ')
 endif
  NULLIFY(grid%accanhs)
ENDIF
IF ( ASSOCIATED( grid%soilenergy ) ) THEN 
  DEALLOCATE(grid%soilenergy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19389,&
'frame/module_domain.f: Failed to deallocate grid%soilenergy. ')
 endif
  NULLIFY(grid%soilenergy)
ENDIF
IF ( ASSOCIATED( grid%snowenergy ) ) THEN 
  DEALLOCATE(grid%snowenergy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19397,&
'frame/module_domain.f: Failed to deallocate grid%snowenergy. ')
 endif
  NULLIFY(grid%snowenergy)
ENDIF
IF ( ASSOCIATED( grid%acc_ssoil ) ) THEN 
  DEALLOCATE(grid%acc_ssoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19405,&
'frame/module_domain.f: Failed to deallocate grid%acc_ssoil. ')
 endif
  NULLIFY(grid%acc_ssoil)
ENDIF
IF ( ASSOCIATED( grid%acc_qinsur ) ) THEN 
  DEALLOCATE(grid%acc_qinsur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19413,&
'frame/module_domain.f: Failed to deallocate grid%acc_qinsur. ')
 endif
  NULLIFY(grid%acc_qinsur)
ENDIF
IF ( ASSOCIATED( grid%acc_qseva ) ) THEN 
  DEALLOCATE(grid%acc_qseva,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19421,&
'frame/module_domain.f: Failed to deallocate grid%acc_qseva. ')
 endif
  NULLIFY(grid%acc_qseva)
ENDIF
IF ( ASSOCIATED( grid%acc_etrani ) ) THEN 
  DEALLOCATE(grid%acc_etrani,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19429,&
'frame/module_domain.f: Failed to deallocate grid%acc_etrani. ')
 endif
  NULLIFY(grid%acc_etrani)
ENDIF
IF ( ASSOCIATED( grid%aceflxb ) ) THEN 
  DEALLOCATE(grid%aceflxb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19437,&
'frame/module_domain.f: Failed to deallocate grid%aceflxb. ')
 endif
  NULLIFY(grid%aceflxb)
ENDIF
IF ( ASSOCIATED( grid%eflxbxy ) ) THEN 
  DEALLOCATE(grid%eflxbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19445,&
'frame/module_domain.f: Failed to deallocate grid%eflxbxy. ')
 endif
  NULLIFY(grid%eflxbxy)
ENDIF
IF ( ASSOCIATED( grid%acc_dwaterxy ) ) THEN 
  DEALLOCATE(grid%acc_dwaterxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19453,&
'frame/module_domain.f: Failed to deallocate grid%acc_dwaterxy. ')
 endif
  NULLIFY(grid%acc_dwaterxy)
ENDIF
IF ( ASSOCIATED( grid%acc_prcpxy ) ) THEN 
  DEALLOCATE(grid%acc_prcpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19461,&
'frame/module_domain.f: Failed to deallocate grid%acc_prcpxy. ')
 endif
  NULLIFY(grid%acc_prcpxy)
ENDIF
IF ( ASSOCIATED( grid%acc_ecanxy ) ) THEN 
  DEALLOCATE(grid%acc_ecanxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19469,&
'frame/module_domain.f: Failed to deallocate grid%acc_ecanxy. ')
 endif
  NULLIFY(grid%acc_ecanxy)
ENDIF
IF ( ASSOCIATED( grid%acc_etranxy ) ) THEN 
  DEALLOCATE(grid%acc_etranxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19477,&
'frame/module_domain.f: Failed to deallocate grid%acc_etranxy. ')
 endif
  NULLIFY(grid%acc_etranxy)
ENDIF
IF ( ASSOCIATED( grid%acc_edirxy ) ) THEN 
  DEALLOCATE(grid%acc_edirxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19485,&
'frame/module_domain.f: Failed to deallocate grid%acc_edirxy. ')
 endif
  NULLIFY(grid%acc_edirxy)
ENDIF
IF ( ASSOCIATED( grid%grainxy ) ) THEN 
  DEALLOCATE(grid%grainxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19493,&
'frame/module_domain.f: Failed to deallocate grid%grainxy. ')
 endif
  NULLIFY(grid%grainxy)
ENDIF
IF ( ASSOCIATED( grid%gddxy ) ) THEN 
  DEALLOCATE(grid%gddxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19501,&
'frame/module_domain.f: Failed to deallocate grid%gddxy. ')
 endif
  NULLIFY(grid%gddxy)
ENDIF
IF ( ASSOCIATED( grid%croptype ) ) THEN 
  DEALLOCATE(grid%croptype,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19509,&
'frame/module_domain.f: Failed to deallocate grid%croptype. ')
 endif
  NULLIFY(grid%croptype)
ENDIF
IF ( ASSOCIATED( grid%planting ) ) THEN 
  DEALLOCATE(grid%planting,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19517,&
'frame/module_domain.f: Failed to deallocate grid%planting. ')
 endif
  NULLIFY(grid%planting)
ENDIF
IF ( ASSOCIATED( grid%harvest ) ) THEN 
  DEALLOCATE(grid%harvest,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19525,&
'frame/module_domain.f: Failed to deallocate grid%harvest. ')
 endif
  NULLIFY(grid%harvest)
ENDIF
IF ( ASSOCIATED( grid%season_gdd ) ) THEN 
  DEALLOCATE(grid%season_gdd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19533,&
'frame/module_domain.f: Failed to deallocate grid%season_gdd. ')
 endif
  NULLIFY(grid%season_gdd)
ENDIF
IF ( ASSOCIATED( grid%cropcat ) ) THEN 
  DEALLOCATE(grid%cropcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19541,&
'frame/module_domain.f: Failed to deallocate grid%cropcat. ')
 endif
  NULLIFY(grid%cropcat)
ENDIF
IF ( ASSOCIATED( grid%pgsxy ) ) THEN 
  DEALLOCATE(grid%pgsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19549,&
'frame/module_domain.f: Failed to deallocate grid%pgsxy. ')
 endif
  NULLIFY(grid%pgsxy)
ENDIF
IF ( ASSOCIATED( grid%gecros_state ) ) THEN 
  DEALLOCATE(grid%gecros_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19557,&
'frame/module_domain.f: Failed to deallocate grid%gecros_state. ')
 endif
  NULLIFY(grid%gecros_state)
ENDIF
IF ( ASSOCIATED( grid%td_fraction ) ) THEN 
  DEALLOCATE(grid%td_fraction,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19565,&
'frame/module_domain.f: Failed to deallocate grid%td_fraction. ')
 endif
  NULLIFY(grid%td_fraction)
ENDIF
IF ( ASSOCIATED( grid%qtdrain ) ) THEN 
  DEALLOCATE(grid%qtdrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19573,&
'frame/module_domain.f: Failed to deallocate grid%qtdrain. ')
 endif
  NULLIFY(grid%qtdrain)
ENDIF
IF ( ASSOCIATED( grid%irfract ) ) THEN 
  DEALLOCATE(grid%irfract,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19581,&
'frame/module_domain.f: Failed to deallocate grid%irfract. ')
 endif
  NULLIFY(grid%irfract)
ENDIF
IF ( ASSOCIATED( grid%sifract ) ) THEN 
  DEALLOCATE(grid%sifract,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19589,&
'frame/module_domain.f: Failed to deallocate grid%sifract. ')
 endif
  NULLIFY(grid%sifract)
ENDIF
IF ( ASSOCIATED( grid%mifract ) ) THEN 
  DEALLOCATE(grid%mifract,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19597,&
'frame/module_domain.f: Failed to deallocate grid%mifract. ')
 endif
  NULLIFY(grid%mifract)
ENDIF
IF ( ASSOCIATED( grid%fifract ) ) THEN 
  DEALLOCATE(grid%fifract,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19605,&
'frame/module_domain.f: Failed to deallocate grid%fifract. ')
 endif
  NULLIFY(grid%fifract)
ENDIF
IF ( ASSOCIATED( grid%irnumsi ) ) THEN 
  DEALLOCATE(grid%irnumsi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19613,&
'frame/module_domain.f: Failed to deallocate grid%irnumsi. ')
 endif
  NULLIFY(grid%irnumsi)
ENDIF
IF ( ASSOCIATED( grid%irnummi ) ) THEN 
  DEALLOCATE(grid%irnummi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19621,&
'frame/module_domain.f: Failed to deallocate grid%irnummi. ')
 endif
  NULLIFY(grid%irnummi)
ENDIF
IF ( ASSOCIATED( grid%irnumfi ) ) THEN 
  DEALLOCATE(grid%irnumfi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19629,&
'frame/module_domain.f: Failed to deallocate grid%irnumfi. ')
 endif
  NULLIFY(grid%irnumfi)
ENDIF
IF ( ASSOCIATED( grid%irwatsi ) ) THEN 
  DEALLOCATE(grid%irwatsi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19637,&
'frame/module_domain.f: Failed to deallocate grid%irwatsi. ')
 endif
  NULLIFY(grid%irwatsi)
ENDIF
IF ( ASSOCIATED( grid%irwatmi ) ) THEN 
  DEALLOCATE(grid%irwatmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19645,&
'frame/module_domain.f: Failed to deallocate grid%irwatmi. ')
 endif
  NULLIFY(grid%irwatmi)
ENDIF
IF ( ASSOCIATED( grid%irwatfi ) ) THEN 
  DEALLOCATE(grid%irwatfi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19653,&
'frame/module_domain.f: Failed to deallocate grid%irwatfi. ')
 endif
  NULLIFY(grid%irwatfi)
ENDIF
IF ( ASSOCIATED( grid%irsivol ) ) THEN 
  DEALLOCATE(grid%irsivol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19661,&
'frame/module_domain.f: Failed to deallocate grid%irsivol. ')
 endif
  NULLIFY(grid%irsivol)
ENDIF
IF ( ASSOCIATED( grid%irmivol ) ) THEN 
  DEALLOCATE(grid%irmivol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19669,&
'frame/module_domain.f: Failed to deallocate grid%irmivol. ')
 endif
  NULLIFY(grid%irmivol)
ENDIF
IF ( ASSOCIATED( grid%irfivol ) ) THEN 
  DEALLOCATE(grid%irfivol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19677,&
'frame/module_domain.f: Failed to deallocate grid%irfivol. ')
 endif
  NULLIFY(grid%irfivol)
ENDIF
IF ( ASSOCIATED( grid%ireloss ) ) THEN 
  DEALLOCATE(grid%ireloss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19685,&
'frame/module_domain.f: Failed to deallocate grid%ireloss. ')
 endif
  NULLIFY(grid%ireloss)
ENDIF
IF ( ASSOCIATED( grid%irrsplh ) ) THEN 
  DEALLOCATE(grid%irrsplh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19693,&
'frame/module_domain.f: Failed to deallocate grid%irrsplh. ')
 endif
  NULLIFY(grid%irrsplh)
ENDIF
IF ( ASSOCIATED( grid%kext_ql ) ) THEN 
  DEALLOCATE(grid%kext_ql,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19701,&
'frame/module_domain.f: Failed to deallocate grid%kext_ql. ')
 endif
  NULLIFY(grid%kext_ql)
ENDIF
IF ( ASSOCIATED( grid%kext_qic ) ) THEN 
  DEALLOCATE(grid%kext_qic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19709,&
'frame/module_domain.f: Failed to deallocate grid%kext_qic. ')
 endif
  NULLIFY(grid%kext_qic)
ENDIF
IF ( ASSOCIATED( grid%kext_qip ) ) THEN 
  DEALLOCATE(grid%kext_qip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19717,&
'frame/module_domain.f: Failed to deallocate grid%kext_qip. ')
 endif
  NULLIFY(grid%kext_qip)
ENDIF
IF ( ASSOCIATED( grid%kext_qid ) ) THEN 
  DEALLOCATE(grid%kext_qid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19725,&
'frame/module_domain.f: Failed to deallocate grid%kext_qid. ')
 endif
  NULLIFY(grid%kext_qid)
ENDIF
IF ( ASSOCIATED( grid%kext_qs ) ) THEN 
  DEALLOCATE(grid%kext_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19733,&
'frame/module_domain.f: Failed to deallocate grid%kext_qs. ')
 endif
  NULLIFY(grid%kext_qs)
ENDIF
IF ( ASSOCIATED( grid%kext_qg ) ) THEN 
  DEALLOCATE(grid%kext_qg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19741,&
'frame/module_domain.f: Failed to deallocate grid%kext_qg. ')
 endif
  NULLIFY(grid%kext_qg)
ENDIF
IF ( ASSOCIATED( grid%kext_qh ) ) THEN 
  DEALLOCATE(grid%kext_qh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19749,&
'frame/module_domain.f: Failed to deallocate grid%kext_qh. ')
 endif
  NULLIFY(grid%kext_qh)
ENDIF
IF ( ASSOCIATED( grid%kext_qa ) ) THEN 
  DEALLOCATE(grid%kext_qa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19757,&
'frame/module_domain.f: Failed to deallocate grid%kext_qa. ')
 endif
  NULLIFY(grid%kext_qa)
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qic ) ) THEN 
  DEALLOCATE(grid%kext_ft_qic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19765,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qic. ')
 endif
  NULLIFY(grid%kext_ft_qic)
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qip ) ) THEN 
  DEALLOCATE(grid%kext_ft_qip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19773,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qip. ')
 endif
  NULLIFY(grid%kext_ft_qip)
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qid ) ) THEN 
  DEALLOCATE(grid%kext_ft_qid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19781,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qid. ')
 endif
  NULLIFY(grid%kext_ft_qid)
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qs ) ) THEN 
  DEALLOCATE(grid%kext_ft_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19789,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qs. ')
 endif
  NULLIFY(grid%kext_ft_qs)
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qg ) ) THEN 
  DEALLOCATE(grid%kext_ft_qg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19797,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qg. ')
 endif
  NULLIFY(grid%kext_ft_qg)
ENDIF
IF ( ASSOCIATED( grid%height ) ) THEN 
  DEALLOCATE(grid%height,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19805,&
'frame/module_domain.f: Failed to deallocate grid%height. ')
 endif
  NULLIFY(grid%height)
ENDIF
IF ( ASSOCIATED( grid%tempc ) ) THEN 
  DEALLOCATE(grid%tempc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19813,&
'frame/module_domain.f: Failed to deallocate grid%tempc. ')
 endif
  NULLIFY(grid%tempc)
ENDIF
IF ( ASSOCIATED( grid%sbmradar ) ) THEN 
  DEALLOCATE(grid%sbmradar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19821,&
'frame/module_domain.f: Failed to deallocate grid%sbmradar. ')
 endif
  NULLIFY(grid%sbmradar)
ENDIF
IF ( ASSOCIATED( grid%tcoli_max ) ) THEN 
  DEALLOCATE(grid%tcoli_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19829,&
'frame/module_domain.f: Failed to deallocate grid%tcoli_max. ')
 endif
  NULLIFY(grid%tcoli_max)
ENDIF
IF ( ASSOCIATED( grid%grpl_flx_max ) ) THEN 
  DEALLOCATE(grid%grpl_flx_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19837,&
'frame/module_domain.f: Failed to deallocate grid%grpl_flx_max. ')
 endif
  NULLIFY(grid%grpl_flx_max)
ENDIF
IF ( ASSOCIATED( grid%refd_com ) ) THEN 
  DEALLOCATE(grid%refd_com,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19845,&
'frame/module_domain.f: Failed to deallocate grid%refd_com. ')
 endif
  NULLIFY(grid%refd_com)
ENDIF
IF ( ASSOCIATED( grid%refd ) ) THEN 
  DEALLOCATE(grid%refd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19853,&
'frame/module_domain.f: Failed to deallocate grid%refd. ')
 endif
  NULLIFY(grid%refd)
ENDIF
IF ( ASSOCIATED( grid%vil ) ) THEN 
  DEALLOCATE(grid%vil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19861,&
'frame/module_domain.f: Failed to deallocate grid%vil. ')
 endif
  NULLIFY(grid%vil)
ENDIF
IF ( ASSOCIATED( grid%radarvil ) ) THEN 
  DEALLOCATE(grid%radarvil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19869,&
'frame/module_domain.f: Failed to deallocate grid%radarvil. ')
 endif
  NULLIFY(grid%radarvil)
ENDIF
IF ( ASSOCIATED( grid%echotop ) ) THEN 
  DEALLOCATE(grid%echotop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19877,&
'frame/module_domain.f: Failed to deallocate grid%echotop. ')
 endif
  NULLIFY(grid%echotop)
ENDIF
IF ( ASSOCIATED( grid%fzlev ) ) THEN 
  DEALLOCATE(grid%fzlev,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19885,&
'frame/module_domain.f: Failed to deallocate grid%fzlev. ')
 endif
  NULLIFY(grid%fzlev)
ENDIF
IF ( ASSOCIATED( grid%icingtop ) ) THEN 
  DEALLOCATE(grid%icingtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19893,&
'frame/module_domain.f: Failed to deallocate grid%icingtop. ')
 endif
  NULLIFY(grid%icingtop)
ENDIF
IF ( ASSOCIATED( grid%icingbot ) ) THEN 
  DEALLOCATE(grid%icingbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19901,&
'frame/module_domain.f: Failed to deallocate grid%icingbot. ')
 endif
  NULLIFY(grid%icingbot)
ENDIF
IF ( ASSOCIATED( grid%qicing_lg ) ) THEN 
  DEALLOCATE(grid%qicing_lg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19909,&
'frame/module_domain.f: Failed to deallocate grid%qicing_lg. ')
 endif
  NULLIFY(grid%qicing_lg)
ENDIF
IF ( ASSOCIATED( grid%qicing_sm ) ) THEN 
  DEALLOCATE(grid%qicing_sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19917,&
'frame/module_domain.f: Failed to deallocate grid%qicing_sm. ')
 endif
  NULLIFY(grid%qicing_sm)
ENDIF
IF ( ASSOCIATED( grid%qicing_lg_max ) ) THEN 
  DEALLOCATE(grid%qicing_lg_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19925,&
'frame/module_domain.f: Failed to deallocate grid%qicing_lg_max. ')
 endif
  NULLIFY(grid%qicing_lg_max)
ENDIF
IF ( ASSOCIATED( grid%qicing_sm_max ) ) THEN 
  DEALLOCATE(grid%qicing_sm_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19933,&
'frame/module_domain.f: Failed to deallocate grid%qicing_sm_max. ')
 endif
  NULLIFY(grid%qicing_sm_max)
ENDIF
IF ( ASSOCIATED( grid%icing_lg ) ) THEN 
  DEALLOCATE(grid%icing_lg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19941,&
'frame/module_domain.f: Failed to deallocate grid%icing_lg. ')
 endif
  NULLIFY(grid%icing_lg)
ENDIF
IF ( ASSOCIATED( grid%icing_sm ) ) THEN 
  DEALLOCATE(grid%icing_sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19949,&
'frame/module_domain.f: Failed to deallocate grid%icing_sm. ')
 endif
  NULLIFY(grid%icing_sm)
ENDIF
IF ( ASSOCIATED( grid%afwa_mslp ) ) THEN 
  DEALLOCATE(grid%afwa_mslp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19957,&
'frame/module_domain.f: Failed to deallocate grid%afwa_mslp. ')
 endif
  NULLIFY(grid%afwa_mslp)
ENDIF
IF ( ASSOCIATED( grid%afwa_heatidx ) ) THEN 
  DEALLOCATE(grid%afwa_heatidx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19965,&
'frame/module_domain.f: Failed to deallocate grid%afwa_heatidx. ')
 endif
  NULLIFY(grid%afwa_heatidx)
ENDIF
IF ( ASSOCIATED( grid%afwa_wchill ) ) THEN 
  DEALLOCATE(grid%afwa_wchill,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19973,&
'frame/module_domain.f: Failed to deallocate grid%afwa_wchill. ')
 endif
  NULLIFY(grid%afwa_wchill)
ENDIF
IF ( ASSOCIATED( grid%afwa_fits ) ) THEN 
  DEALLOCATE(grid%afwa_fits,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19981,&
'frame/module_domain.f: Failed to deallocate grid%afwa_fits. ')
 endif
  NULLIFY(grid%afwa_fits)
ENDIF
IF ( ASSOCIATED( grid%afwa_tlyrbot ) ) THEN 
  DEALLOCATE(grid%afwa_tlyrbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19989,&
'frame/module_domain.f: Failed to deallocate grid%afwa_tlyrbot. ')
 endif
  NULLIFY(grid%afwa_tlyrbot)
ENDIF
IF ( ASSOCIATED( grid%afwa_tlyrtop ) ) THEN 
  DEALLOCATE(grid%afwa_tlyrtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",19997,&
'frame/module_domain.f: Failed to deallocate grid%afwa_tlyrtop. ')
 endif
  NULLIFY(grid%afwa_tlyrtop)
ENDIF
IF ( ASSOCIATED( grid%afwa_turb ) ) THEN 
  DEALLOCATE(grid%afwa_turb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20005,&
'frame/module_domain.f: Failed to deallocate grid%afwa_turb. ')
 endif
  NULLIFY(grid%afwa_turb)
ENDIF
IF ( ASSOCIATED( grid%afwa_llturb ) ) THEN 
  DEALLOCATE(grid%afwa_llturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20013,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturb. ')
 endif
  NULLIFY(grid%afwa_llturb)
ENDIF
IF ( ASSOCIATED( grid%afwa_llturblgt ) ) THEN 
  DEALLOCATE(grid%afwa_llturblgt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20021,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturblgt. ')
 endif
  NULLIFY(grid%afwa_llturblgt)
ENDIF
IF ( ASSOCIATED( grid%afwa_llturbmdt ) ) THEN 
  DEALLOCATE(grid%afwa_llturbmdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20029,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturbmdt. ')
 endif
  NULLIFY(grid%afwa_llturbmdt)
ENDIF
IF ( ASSOCIATED( grid%afwa_llturbsvr ) ) THEN 
  DEALLOCATE(grid%afwa_llturbsvr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20037,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturbsvr. ')
 endif
  NULLIFY(grid%afwa_llturbsvr)
ENDIF
IF ( ASSOCIATED( grid%afwa_precip ) ) THEN 
  DEALLOCATE(grid%afwa_precip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20045,&
'frame/module_domain.f: Failed to deallocate grid%afwa_precip. ')
 endif
  NULLIFY(grid%afwa_precip)
ENDIF
IF ( ASSOCIATED( grid%afwa_totprecip ) ) THEN 
  DEALLOCATE(grid%afwa_totprecip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20053,&
'frame/module_domain.f: Failed to deallocate grid%afwa_totprecip. ')
 endif
  NULLIFY(grid%afwa_totprecip)
ENDIF
IF ( ASSOCIATED( grid%afwa_rain ) ) THEN 
  DEALLOCATE(grid%afwa_rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20061,&
'frame/module_domain.f: Failed to deallocate grid%afwa_rain. ')
 endif
  NULLIFY(grid%afwa_rain)
ENDIF
IF ( ASSOCIATED( grid%afwa_snow ) ) THEN 
  DEALLOCATE(grid%afwa_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20069,&
'frame/module_domain.f: Failed to deallocate grid%afwa_snow. ')
 endif
  NULLIFY(grid%afwa_snow)
ENDIF
IF ( ASSOCIATED( grid%afwa_ice ) ) THEN 
  DEALLOCATE(grid%afwa_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20077,&
'frame/module_domain.f: Failed to deallocate grid%afwa_ice. ')
 endif
  NULLIFY(grid%afwa_ice)
ENDIF
IF ( ASSOCIATED( grid%afwa_fzra ) ) THEN 
  DEALLOCATE(grid%afwa_fzra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20085,&
'frame/module_domain.f: Failed to deallocate grid%afwa_fzra. ')
 endif
  NULLIFY(grid%afwa_fzra)
ENDIF
IF ( ASSOCIATED( grid%afwa_snowfall ) ) THEN 
  DEALLOCATE(grid%afwa_snowfall,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20093,&
'frame/module_domain.f: Failed to deallocate grid%afwa_snowfall. ')
 endif
  NULLIFY(grid%afwa_snowfall)
ENDIF
IF ( ASSOCIATED( grid%afwa_vis ) ) THEN 
  DEALLOCATE(grid%afwa_vis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20101,&
'frame/module_domain.f: Failed to deallocate grid%afwa_vis. ')
 endif
  NULLIFY(grid%afwa_vis)
ENDIF
IF ( ASSOCIATED( grid%afwa_vis_alpha ) ) THEN 
  DEALLOCATE(grid%afwa_vis_alpha,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20109,&
'frame/module_domain.f: Failed to deallocate grid%afwa_vis_alpha. ')
 endif
  NULLIFY(grid%afwa_vis_alpha)
ENDIF
IF ( ASSOCIATED( grid%afwa_vis_dust ) ) THEN 
  DEALLOCATE(grid%afwa_vis_dust,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20117,&
'frame/module_domain.f: Failed to deallocate grid%afwa_vis_dust. ')
 endif
  NULLIFY(grid%afwa_vis_dust)
ENDIF
IF ( ASSOCIATED( grid%afwa_cloud ) ) THEN 
  DEALLOCATE(grid%afwa_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20125,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cloud. ')
 endif
  NULLIFY(grid%afwa_cloud)
ENDIF
IF ( ASSOCIATED( grid%afwa_cloud_ceil ) ) THEN 
  DEALLOCATE(grid%afwa_cloud_ceil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20133,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cloud_ceil. ')
 endif
  NULLIFY(grid%afwa_cloud_ceil)
ENDIF
IF ( ASSOCIATED( grid%afwa_cape ) ) THEN 
  DEALLOCATE(grid%afwa_cape,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20141,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cape. ')
 endif
  NULLIFY(grid%afwa_cape)
ENDIF
IF ( ASSOCIATED( grid%afwa_cin ) ) THEN 
  DEALLOCATE(grid%afwa_cin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20149,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cin. ')
 endif
  NULLIFY(grid%afwa_cin)
ENDIF
IF ( ASSOCIATED( grid%afwa_cape_mu ) ) THEN 
  DEALLOCATE(grid%afwa_cape_mu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20157,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cape_mu. ')
 endif
  NULLIFY(grid%afwa_cape_mu)
ENDIF
IF ( ASSOCIATED( grid%afwa_cin_mu ) ) THEN 
  DEALLOCATE(grid%afwa_cin_mu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20165,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cin_mu. ')
 endif
  NULLIFY(grid%afwa_cin_mu)
ENDIF
IF ( ASSOCIATED( grid%afwa_zlfc ) ) THEN 
  DEALLOCATE(grid%afwa_zlfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20173,&
'frame/module_domain.f: Failed to deallocate grid%afwa_zlfc. ')
 endif
  NULLIFY(grid%afwa_zlfc)
ENDIF
IF ( ASSOCIATED( grid%afwa_plfc ) ) THEN 
  DEALLOCATE(grid%afwa_plfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20181,&
'frame/module_domain.f: Failed to deallocate grid%afwa_plfc. ')
 endif
  NULLIFY(grid%afwa_plfc)
ENDIF
IF ( ASSOCIATED( grid%afwa_lidx ) ) THEN 
  DEALLOCATE(grid%afwa_lidx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20189,&
'frame/module_domain.f: Failed to deallocate grid%afwa_lidx. ')
 endif
  NULLIFY(grid%afwa_lidx)
ENDIF
IF ( ASSOCIATED( grid%afwa_pwat ) ) THEN 
  DEALLOCATE(grid%afwa_pwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20197,&
'frame/module_domain.f: Failed to deallocate grid%afwa_pwat. ')
 endif
  NULLIFY(grid%afwa_pwat)
ENDIF
IF ( ASSOCIATED( grid%midrh_min ) ) THEN 
  DEALLOCATE(grid%midrh_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20205,&
'frame/module_domain.f: Failed to deallocate grid%midrh_min. ')
 endif
  NULLIFY(grid%midrh_min)
ENDIF
IF ( ASSOCIATED( grid%midrh_min_old ) ) THEN 
  DEALLOCATE(grid%midrh_min_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20213,&
'frame/module_domain.f: Failed to deallocate grid%midrh_min_old. ')
 endif
  NULLIFY(grid%midrh_min_old)
ENDIF
IF ( ASSOCIATED( grid%afwa_hail ) ) THEN 
  DEALLOCATE(grid%afwa_hail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20221,&
'frame/module_domain.f: Failed to deallocate grid%afwa_hail. ')
 endif
  NULLIFY(grid%afwa_hail)
ENDIF
IF ( ASSOCIATED( grid%afwa_llws ) ) THEN 
  DEALLOCATE(grid%afwa_llws,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20229,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llws. ')
 endif
  NULLIFY(grid%afwa_llws)
ENDIF
IF ( ASSOCIATED( grid%afwa_tornado ) ) THEN 
  DEALLOCATE(grid%afwa_tornado,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20237,&
'frame/module_domain.f: Failed to deallocate grid%afwa_tornado. ')
 endif
  NULLIFY(grid%afwa_tornado)
ENDIF
IF ( ASSOCIATED( grid%tornado_mask ) ) THEN 
  DEALLOCATE(grid%tornado_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20245,&
'frame/module_domain.f: Failed to deallocate grid%tornado_mask. ')
 endif
  NULLIFY(grid%tornado_mask)
ENDIF
IF ( ASSOCIATED( grid%tornado_dur ) ) THEN 
  DEALLOCATE(grid%tornado_dur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20253,&
'frame/module_domain.f: Failed to deallocate grid%tornado_dur. ')
 endif
  NULLIFY(grid%tornado_dur)
ENDIF
IF ( ASSOCIATED( grid%psfc_mean ) ) THEN 
  DEALLOCATE(grid%psfc_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20261,&
'frame/module_domain.f: Failed to deallocate grid%psfc_mean. ')
 endif
  NULLIFY(grid%psfc_mean)
ENDIF
IF ( ASSOCIATED( grid%tsk_mean ) ) THEN 
  DEALLOCATE(grid%tsk_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20269,&
'frame/module_domain.f: Failed to deallocate grid%tsk_mean. ')
 endif
  NULLIFY(grid%tsk_mean)
ENDIF
IF ( ASSOCIATED( grid%pmsl_mean ) ) THEN 
  DEALLOCATE(grid%pmsl_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20277,&
'frame/module_domain.f: Failed to deallocate grid%pmsl_mean. ')
 endif
  NULLIFY(grid%pmsl_mean)
ENDIF
IF ( ASSOCIATED( grid%t2_mean ) ) THEN 
  DEALLOCATE(grid%t2_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20285,&
'frame/module_domain.f: Failed to deallocate grid%t2_mean. ')
 endif
  NULLIFY(grid%t2_mean)
ENDIF
IF ( ASSOCIATED( grid%th2_mean ) ) THEN 
  DEALLOCATE(grid%th2_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20293,&
'frame/module_domain.f: Failed to deallocate grid%th2_mean. ')
 endif
  NULLIFY(grid%th2_mean)
ENDIF
IF ( ASSOCIATED( grid%q2_mean ) ) THEN 
  DEALLOCATE(grid%q2_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20301,&
'frame/module_domain.f: Failed to deallocate grid%q2_mean. ')
 endif
  NULLIFY(grid%q2_mean)
ENDIF
IF ( ASSOCIATED( grid%u10_mean ) ) THEN 
  DEALLOCATE(grid%u10_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20309,&
'frame/module_domain.f: Failed to deallocate grid%u10_mean. ')
 endif
  NULLIFY(grid%u10_mean)
ENDIF
IF ( ASSOCIATED( grid%v10_mean ) ) THEN 
  DEALLOCATE(grid%v10_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20317,&
'frame/module_domain.f: Failed to deallocate grid%v10_mean. ')
 endif
  NULLIFY(grid%v10_mean)
ENDIF
IF ( ASSOCIATED( grid%hfx_mean ) ) THEN 
  DEALLOCATE(grid%hfx_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20325,&
'frame/module_domain.f: Failed to deallocate grid%hfx_mean. ')
 endif
  NULLIFY(grid%hfx_mean)
ENDIF
IF ( ASSOCIATED( grid%lh_mean ) ) THEN 
  DEALLOCATE(grid%lh_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20333,&
'frame/module_domain.f: Failed to deallocate grid%lh_mean. ')
 endif
  NULLIFY(grid%lh_mean)
ENDIF
IF ( ASSOCIATED( grid%swdnb_mean ) ) THEN 
  DEALLOCATE(grid%swdnb_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20341,&
'frame/module_domain.f: Failed to deallocate grid%swdnb_mean. ')
 endif
  NULLIFY(grid%swdnb_mean)
ENDIF
IF ( ASSOCIATED( grid%glw_mean ) ) THEN 
  DEALLOCATE(grid%glw_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20349,&
'frame/module_domain.f: Failed to deallocate grid%glw_mean. ')
 endif
  NULLIFY(grid%glw_mean)
ENDIF
IF ( ASSOCIATED( grid%lwupb_mean ) ) THEN 
  DEALLOCATE(grid%lwupb_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20357,&
'frame/module_domain.f: Failed to deallocate grid%lwupb_mean. ')
 endif
  NULLIFY(grid%lwupb_mean)
ENDIF
IF ( ASSOCIATED( grid%swupb_mean ) ) THEN 
  DEALLOCATE(grid%swupb_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20365,&
'frame/module_domain.f: Failed to deallocate grid%swupb_mean. ')
 endif
  NULLIFY(grid%swupb_mean)
ENDIF
IF ( ASSOCIATED( grid%swupt_mean ) ) THEN 
  DEALLOCATE(grid%swupt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20373,&
'frame/module_domain.f: Failed to deallocate grid%swupt_mean. ')
 endif
  NULLIFY(grid%swupt_mean)
ENDIF
IF ( ASSOCIATED( grid%swdnt_mean ) ) THEN 
  DEALLOCATE(grid%swdnt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20381,&
'frame/module_domain.f: Failed to deallocate grid%swdnt_mean. ')
 endif
  NULLIFY(grid%swdnt_mean)
ENDIF
IF ( ASSOCIATED( grid%lwupt_mean ) ) THEN 
  DEALLOCATE(grid%lwupt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20389,&
'frame/module_domain.f: Failed to deallocate grid%lwupt_mean. ')
 endif
  NULLIFY(grid%lwupt_mean)
ENDIF
IF ( ASSOCIATED( grid%lwdnt_mean ) ) THEN 
  DEALLOCATE(grid%lwdnt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20397,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt_mean. ')
 endif
  NULLIFY(grid%lwdnt_mean)
ENDIF
IF ( ASSOCIATED( grid%psfc_diurn ) ) THEN 
  DEALLOCATE(grid%psfc_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20405,&
'frame/module_domain.f: Failed to deallocate grid%psfc_diurn. ')
 endif
  NULLIFY(grid%psfc_diurn)
ENDIF
IF ( ASSOCIATED( grid%tsk_diurn ) ) THEN 
  DEALLOCATE(grid%tsk_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20413,&
'frame/module_domain.f: Failed to deallocate grid%tsk_diurn. ')
 endif
  NULLIFY(grid%tsk_diurn)
ENDIF
IF ( ASSOCIATED( grid%t2_diurn ) ) THEN 
  DEALLOCATE(grid%t2_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20421,&
'frame/module_domain.f: Failed to deallocate grid%t2_diurn. ')
 endif
  NULLIFY(grid%t2_diurn)
ENDIF
IF ( ASSOCIATED( grid%th2_diurn ) ) THEN 
  DEALLOCATE(grid%th2_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20429,&
'frame/module_domain.f: Failed to deallocate grid%th2_diurn. ')
 endif
  NULLIFY(grid%th2_diurn)
ENDIF
IF ( ASSOCIATED( grid%q2_diurn ) ) THEN 
  DEALLOCATE(grid%q2_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20437,&
'frame/module_domain.f: Failed to deallocate grid%q2_diurn. ')
 endif
  NULLIFY(grid%q2_diurn)
ENDIF
IF ( ASSOCIATED( grid%u10_diurn ) ) THEN 
  DEALLOCATE(grid%u10_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20445,&
'frame/module_domain.f: Failed to deallocate grid%u10_diurn. ')
 endif
  NULLIFY(grid%u10_diurn)
ENDIF
IF ( ASSOCIATED( grid%v10_diurn ) ) THEN 
  DEALLOCATE(grid%v10_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20453,&
'frame/module_domain.f: Failed to deallocate grid%v10_diurn. ')
 endif
  NULLIFY(grid%v10_diurn)
ENDIF
IF ( ASSOCIATED( grid%hfx_diurn ) ) THEN 
  DEALLOCATE(grid%hfx_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20461,&
'frame/module_domain.f: Failed to deallocate grid%hfx_diurn. ')
 endif
  NULLIFY(grid%hfx_diurn)
ENDIF
IF ( ASSOCIATED( grid%lh_diurn ) ) THEN 
  DEALLOCATE(grid%lh_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20469,&
'frame/module_domain.f: Failed to deallocate grid%lh_diurn. ')
 endif
  NULLIFY(grid%lh_diurn)
ENDIF
IF ( ASSOCIATED( grid%swdnb_diurn ) ) THEN 
  DEALLOCATE(grid%swdnb_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20477,&
'frame/module_domain.f: Failed to deallocate grid%swdnb_diurn. ')
 endif
  NULLIFY(grid%swdnb_diurn)
ENDIF
IF ( ASSOCIATED( grid%glw_diurn ) ) THEN 
  DEALLOCATE(grid%glw_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20485,&
'frame/module_domain.f: Failed to deallocate grid%glw_diurn. ')
 endif
  NULLIFY(grid%glw_diurn)
ENDIF
IF ( ASSOCIATED( grid%lwupb_diurn ) ) THEN 
  DEALLOCATE(grid%lwupb_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20493,&
'frame/module_domain.f: Failed to deallocate grid%lwupb_diurn. ')
 endif
  NULLIFY(grid%lwupb_diurn)
ENDIF
IF ( ASSOCIATED( grid%swupb_diurn ) ) THEN 
  DEALLOCATE(grid%swupb_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20501,&
'frame/module_domain.f: Failed to deallocate grid%swupb_diurn. ')
 endif
  NULLIFY(grid%swupb_diurn)
ENDIF
IF ( ASSOCIATED( grid%swupt_diurn ) ) THEN 
  DEALLOCATE(grid%swupt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20509,&
'frame/module_domain.f: Failed to deallocate grid%swupt_diurn. ')
 endif
  NULLIFY(grid%swupt_diurn)
ENDIF
IF ( ASSOCIATED( grid%swdnt_diurn ) ) THEN 
  DEALLOCATE(grid%swdnt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20517,&
'frame/module_domain.f: Failed to deallocate grid%swdnt_diurn. ')
 endif
  NULLIFY(grid%swdnt_diurn)
ENDIF
IF ( ASSOCIATED( grid%lwupt_diurn ) ) THEN 
  DEALLOCATE(grid%lwupt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20525,&
'frame/module_domain.f: Failed to deallocate grid%lwupt_diurn. ')
 endif
  NULLIFY(grid%lwupt_diurn)
ENDIF
IF ( ASSOCIATED( grid%lwdnt_diurn ) ) THEN 
  DEALLOCATE(grid%lwdnt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20533,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt_diurn. ')
 endif
  NULLIFY(grid%lwdnt_diurn)
ENDIF
IF ( ASSOCIATED( grid%psfc_dtmp ) ) THEN 
  DEALLOCATE(grid%psfc_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20541,&
'frame/module_domain.f: Failed to deallocate grid%psfc_dtmp. ')
 endif
  NULLIFY(grid%psfc_dtmp)
ENDIF
IF ( ASSOCIATED( grid%tsk_dtmp ) ) THEN 
  DEALLOCATE(grid%tsk_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20549,&
'frame/module_domain.f: Failed to deallocate grid%tsk_dtmp. ')
 endif
  NULLIFY(grid%tsk_dtmp)
ENDIF
IF ( ASSOCIATED( grid%t2_dtmp ) ) THEN 
  DEALLOCATE(grid%t2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20557,&
'frame/module_domain.f: Failed to deallocate grid%t2_dtmp. ')
 endif
  NULLIFY(grid%t2_dtmp)
ENDIF
IF ( ASSOCIATED( grid%th2_dtmp ) ) THEN 
  DEALLOCATE(grid%th2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20565,&
'frame/module_domain.f: Failed to deallocate grid%th2_dtmp. ')
 endif
  NULLIFY(grid%th2_dtmp)
ENDIF
IF ( ASSOCIATED( grid%q2_dtmp ) ) THEN 
  DEALLOCATE(grid%q2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20573,&
'frame/module_domain.f: Failed to deallocate grid%q2_dtmp. ')
 endif
  NULLIFY(grid%q2_dtmp)
ENDIF
IF ( ASSOCIATED( grid%u10_dtmp ) ) THEN 
  DEALLOCATE(grid%u10_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20581,&
'frame/module_domain.f: Failed to deallocate grid%u10_dtmp. ')
 endif
  NULLIFY(grid%u10_dtmp)
ENDIF
IF ( ASSOCIATED( grid%v10_dtmp ) ) THEN 
  DEALLOCATE(grid%v10_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20589,&
'frame/module_domain.f: Failed to deallocate grid%v10_dtmp. ')
 endif
  NULLIFY(grid%v10_dtmp)
ENDIF
IF ( ASSOCIATED( grid%hfx_dtmp ) ) THEN 
  DEALLOCATE(grid%hfx_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20597,&
'frame/module_domain.f: Failed to deallocate grid%hfx_dtmp. ')
 endif
  NULLIFY(grid%hfx_dtmp)
ENDIF
IF ( ASSOCIATED( grid%lh_dtmp ) ) THEN 
  DEALLOCATE(grid%lh_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20605,&
'frame/module_domain.f: Failed to deallocate grid%lh_dtmp. ')
 endif
  NULLIFY(grid%lh_dtmp)
ENDIF
IF ( ASSOCIATED( grid%swdnb_dtmp ) ) THEN 
  DEALLOCATE(grid%swdnb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20613,&
'frame/module_domain.f: Failed to deallocate grid%swdnb_dtmp. ')
 endif
  NULLIFY(grid%swdnb_dtmp)
ENDIF
IF ( ASSOCIATED( grid%glw_dtmp ) ) THEN 
  DEALLOCATE(grid%glw_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20621,&
'frame/module_domain.f: Failed to deallocate grid%glw_dtmp. ')
 endif
  NULLIFY(grid%glw_dtmp)
ENDIF
IF ( ASSOCIATED( grid%lwupb_dtmp ) ) THEN 
  DEALLOCATE(grid%lwupb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20629,&
'frame/module_domain.f: Failed to deallocate grid%lwupb_dtmp. ')
 endif
  NULLIFY(grid%lwupb_dtmp)
ENDIF
IF ( ASSOCIATED( grid%swupb_dtmp ) ) THEN 
  DEALLOCATE(grid%swupb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20637,&
'frame/module_domain.f: Failed to deallocate grid%swupb_dtmp. ')
 endif
  NULLIFY(grid%swupb_dtmp)
ENDIF
IF ( ASSOCIATED( grid%swupt_dtmp ) ) THEN 
  DEALLOCATE(grid%swupt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20645,&
'frame/module_domain.f: Failed to deallocate grid%swupt_dtmp. ')
 endif
  NULLIFY(grid%swupt_dtmp)
ENDIF
IF ( ASSOCIATED( grid%swdnt_dtmp ) ) THEN 
  DEALLOCATE(grid%swdnt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20653,&
'frame/module_domain.f: Failed to deallocate grid%swdnt_dtmp. ')
 endif
  NULLIFY(grid%swdnt_dtmp)
ENDIF
IF ( ASSOCIATED( grid%lwupt_dtmp ) ) THEN 
  DEALLOCATE(grid%lwupt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20661,&
'frame/module_domain.f: Failed to deallocate grid%lwupt_dtmp. ')
 endif
  NULLIFY(grid%lwupt_dtmp)
ENDIF
IF ( ASSOCIATED( grid%lwdnt_dtmp ) ) THEN 
  DEALLOCATE(grid%lwdnt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20669,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt_dtmp. ')
 endif
  NULLIFY(grid%lwdnt_dtmp)
ENDIF
IF ( ASSOCIATED( grid%rscghis_2d ) ) THEN 
  DEALLOCATE(grid%rscghis_2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20677,&
'frame/module_domain.f: Failed to deallocate grid%rscghis_2d. ')
 endif
  NULLIFY(grid%rscghis_2d)
ENDIF
IF ( ASSOCIATED( grid%induc ) ) THEN 
  DEALLOCATE(grid%induc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20685,&
'frame/module_domain.f: Failed to deallocate grid%induc. ')
 endif
  NULLIFY(grid%induc)
ENDIF
IF ( ASSOCIATED( grid%noninduc ) ) THEN 
  DEALLOCATE(grid%noninduc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20693,&
'frame/module_domain.f: Failed to deallocate grid%noninduc. ')
 endif
  NULLIFY(grid%noninduc)
ENDIF
IF ( ASSOCIATED( grid%sctot ) ) THEN 
  DEALLOCATE(grid%sctot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20701,&
'frame/module_domain.f: Failed to deallocate grid%sctot. ')
 endif
  NULLIFY(grid%sctot)
ENDIF
IF ( ASSOCIATED( grid%elecmag ) ) THEN 
  DEALLOCATE(grid%elecmag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20709,&
'frame/module_domain.f: Failed to deallocate grid%elecmag. ')
 endif
  NULLIFY(grid%elecmag)
ENDIF
IF ( ASSOCIATED( grid%elecx ) ) THEN 
  DEALLOCATE(grid%elecx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20717,&
'frame/module_domain.f: Failed to deallocate grid%elecx. ')
 endif
  NULLIFY(grid%elecx)
ENDIF
IF ( ASSOCIATED( grid%elecy ) ) THEN 
  DEALLOCATE(grid%elecy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20725,&
'frame/module_domain.f: Failed to deallocate grid%elecy. ')
 endif
  NULLIFY(grid%elecy)
ENDIF
IF ( ASSOCIATED( grid%elecz ) ) THEN 
  DEALLOCATE(grid%elecz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20733,&
'frame/module_domain.f: Failed to deallocate grid%elecz. ')
 endif
  NULLIFY(grid%elecz)
ENDIF
IF ( ASSOCIATED( grid%pot ) ) THEN 
  DEALLOCATE(grid%pot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20741,&
'frame/module_domain.f: Failed to deallocate grid%pot. ')
 endif
  NULLIFY(grid%pot)
ENDIF
IF ( ASSOCIATED( grid%light ) ) THEN 
  DEALLOCATE(grid%light,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20749,&
'frame/module_domain.f: Failed to deallocate grid%light. ')
 endif
  NULLIFY(grid%light)
ENDIF
IF ( ASSOCIATED( grid%lightdens ) ) THEN 
  DEALLOCATE(grid%lightdens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20757,&
'frame/module_domain.f: Failed to deallocate grid%lightdens. ')
 endif
  NULLIFY(grid%lightdens)
ENDIF
IF ( ASSOCIATED( grid%lightdis ) ) THEN 
  DEALLOCATE(grid%lightdis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20765,&
'frame/module_domain.f: Failed to deallocate grid%lightdis. ')
 endif
  NULLIFY(grid%lightdis)
ENDIF
IF ( ASSOCIATED( grid%flshi ) ) THEN 
  DEALLOCATE(grid%flshi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20773,&
'frame/module_domain.f: Failed to deallocate grid%flshi. ')
 endif
  NULLIFY(grid%flshi)
ENDIF
IF ( ASSOCIATED( grid%flshn ) ) THEN 
  DEALLOCATE(grid%flshn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20781,&
'frame/module_domain.f: Failed to deallocate grid%flshn. ')
 endif
  NULLIFY(grid%flshn)
ENDIF
IF ( ASSOCIATED( grid%flshp ) ) THEN 
  DEALLOCATE(grid%flshp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20789,&
'frame/module_domain.f: Failed to deallocate grid%flshp. ')
 endif
  NULLIFY(grid%flshp)
ENDIF
IF ( ASSOCIATED( grid%field_u_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_u_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20797,&
'frame/module_domain.f: Failed to deallocate grid%field_u_tend_perturb. ')
 endif
  NULLIFY(grid%field_u_tend_perturb)
ENDIF
IF ( ASSOCIATED( grid%field_v_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_v_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20805,&
'frame/module_domain.f: Failed to deallocate grid%field_v_tend_perturb. ')
 endif
  NULLIFY(grid%field_v_tend_perturb)
ENDIF
IF ( ASSOCIATED( grid%field_t_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_t_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20813,&
'frame/module_domain.f: Failed to deallocate grid%field_t_tend_perturb. ')
 endif
  NULLIFY(grid%field_t_tend_perturb)
ENDIF
IF ( ASSOCIATED( grid%c1h ) ) THEN 
  DEALLOCATE(grid%c1h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20821,&
'frame/module_domain.f: Failed to deallocate grid%c1h. ')
 endif
  NULLIFY(grid%c1h)
ENDIF
IF ( ASSOCIATED( grid%c2h ) ) THEN 
  DEALLOCATE(grid%c2h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20829,&
'frame/module_domain.f: Failed to deallocate grid%c2h. ')
 endif
  NULLIFY(grid%c2h)
ENDIF
IF ( ASSOCIATED( grid%c1f ) ) THEN 
  DEALLOCATE(grid%c1f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20837,&
'frame/module_domain.f: Failed to deallocate grid%c1f. ')
 endif
  NULLIFY(grid%c1f)
ENDIF
IF ( ASSOCIATED( grid%c2f ) ) THEN 
  DEALLOCATE(grid%c2f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20845,&
'frame/module_domain.f: Failed to deallocate grid%c2f. ')
 endif
  NULLIFY(grid%c2f)
ENDIF
IF ( ASSOCIATED( grid%c3h ) ) THEN 
  DEALLOCATE(grid%c3h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20853,&
'frame/module_domain.f: Failed to deallocate grid%c3h. ')
 endif
  NULLIFY(grid%c3h)
ENDIF
IF ( ASSOCIATED( grid%c4h ) ) THEN 
  DEALLOCATE(grid%c4h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20861,&
'frame/module_domain.f: Failed to deallocate grid%c4h. ')
 endif
  NULLIFY(grid%c4h)
ENDIF
IF ( ASSOCIATED( grid%c3f ) ) THEN 
  DEALLOCATE(grid%c3f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20869,&
'frame/module_domain.f: Failed to deallocate grid%c3f. ')
 endif
  NULLIFY(grid%c3f)
ENDIF
IF ( ASSOCIATED( grid%c4f ) ) THEN 
  DEALLOCATE(grid%c4f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20877,&
'frame/module_domain.f: Failed to deallocate grid%c4f. ')
 endif
  NULLIFY(grid%c4f)
ENDIF
IF ( ASSOCIATED( grid%pcb ) ) THEN 
  DEALLOCATE(grid%pcb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20885,&
'frame/module_domain.f: Failed to deallocate grid%pcb. ')
 endif
  NULLIFY(grid%pcb)
ENDIF
IF ( ASSOCIATED( grid%pc_1 ) ) THEN 
  DEALLOCATE(grid%pc_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20893,&
'frame/module_domain.f: Failed to deallocate grid%pc_1. ')
 endif
  NULLIFY(grid%pc_1)
ENDIF
IF ( ASSOCIATED( grid%pc_2 ) ) THEN 
  DEALLOCATE(grid%pc_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20901,&
'frame/module_domain.f: Failed to deallocate grid%pc_2. ')
 endif
  NULLIFY(grid%pc_2)
ENDIF
IF ( ASSOCIATED( grid%pc_bxs ) ) THEN 
  DEALLOCATE(grid%pc_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20909,&
'frame/module_domain.f: Failed to deallocate grid%pc_bxs. ')
 endif
  NULLIFY(grid%pc_bxs)
ENDIF
IF ( ASSOCIATED( grid%pc_bxe ) ) THEN 
  DEALLOCATE(grid%pc_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20917,&
'frame/module_domain.f: Failed to deallocate grid%pc_bxe. ')
 endif
  NULLIFY(grid%pc_bxe)
ENDIF
IF ( ASSOCIATED( grid%pc_bys ) ) THEN 
  DEALLOCATE(grid%pc_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20925,&
'frame/module_domain.f: Failed to deallocate grid%pc_bys. ')
 endif
  NULLIFY(grid%pc_bys)
ENDIF
IF ( ASSOCIATED( grid%pc_bye ) ) THEN 
  DEALLOCATE(grid%pc_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20933,&
'frame/module_domain.f: Failed to deallocate grid%pc_bye. ')
 endif
  NULLIFY(grid%pc_bye)
ENDIF
IF ( ASSOCIATED( grid%pc_btxs ) ) THEN 
  DEALLOCATE(grid%pc_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20941,&
'frame/module_domain.f: Failed to deallocate grid%pc_btxs. ')
 endif
  NULLIFY(grid%pc_btxs)
ENDIF
IF ( ASSOCIATED( grid%pc_btxe ) ) THEN 
  DEALLOCATE(grid%pc_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20949,&
'frame/module_domain.f: Failed to deallocate grid%pc_btxe. ')
 endif
  NULLIFY(grid%pc_btxe)
ENDIF
IF ( ASSOCIATED( grid%pc_btys ) ) THEN 
  DEALLOCATE(grid%pc_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20957,&
'frame/module_domain.f: Failed to deallocate grid%pc_btys. ')
 endif
  NULLIFY(grid%pc_btys)
ENDIF
IF ( ASSOCIATED( grid%pc_btye ) ) THEN 
  DEALLOCATE(grid%pc_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20965,&
'frame/module_domain.f: Failed to deallocate grid%pc_btye. ')
 endif
  NULLIFY(grid%pc_btye)
ENDIF
IF ( ASSOCIATED( grid%qnwfa_gc ) ) THEN 
  DEALLOCATE(grid%qnwfa_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20973,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_gc. ')
 endif
  NULLIFY(grid%qnwfa_gc)
ENDIF
IF ( ASSOCIATED( grid%qnifa_gc ) ) THEN 
  DEALLOCATE(grid%qnifa_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20981,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_gc. ')
 endif
  NULLIFY(grid%qnifa_gc)
ENDIF
IF ( ASSOCIATED( grid%qnbca_gc ) ) THEN 
  DEALLOCATE(grid%qnbca_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20989,&
'frame/module_domain.f: Failed to deallocate grid%qnbca_gc. ')
 endif
  NULLIFY(grid%qnbca_gc)
ENDIF
IF ( ASSOCIATED( grid%p_wif_gc ) ) THEN 
  DEALLOCATE(grid%p_wif_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",20997,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_gc. ')
 endif
  NULLIFY(grid%p_wif_gc)
ENDIF
IF ( ASSOCIATED( grid%p_wif_now ) ) THEN 
  DEALLOCATE(grid%p_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21005,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_now. ')
 endif
  NULLIFY(grid%p_wif_now)
ENDIF
IF ( ASSOCIATED( grid%p_wif_jan ) ) THEN 
  DEALLOCATE(grid%p_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21013,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_jan. ')
 endif
  NULLIFY(grid%p_wif_jan)
ENDIF
IF ( ASSOCIATED( grid%p_wif_feb ) ) THEN 
  DEALLOCATE(grid%p_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21021,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_feb. ')
 endif
  NULLIFY(grid%p_wif_feb)
ENDIF
IF ( ASSOCIATED( grid%p_wif_mar ) ) THEN 
  DEALLOCATE(grid%p_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21029,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_mar. ')
 endif
  NULLIFY(grid%p_wif_mar)
ENDIF
IF ( ASSOCIATED( grid%p_wif_apr ) ) THEN 
  DEALLOCATE(grid%p_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21037,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_apr. ')
 endif
  NULLIFY(grid%p_wif_apr)
ENDIF
IF ( ASSOCIATED( grid%p_wif_may ) ) THEN 
  DEALLOCATE(grid%p_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21045,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_may. ')
 endif
  NULLIFY(grid%p_wif_may)
ENDIF
IF ( ASSOCIATED( grid%p_wif_jun ) ) THEN 
  DEALLOCATE(grid%p_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21053,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_jun. ')
 endif
  NULLIFY(grid%p_wif_jun)
ENDIF
IF ( ASSOCIATED( grid%p_wif_jul ) ) THEN 
  DEALLOCATE(grid%p_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21061,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_jul. ')
 endif
  NULLIFY(grid%p_wif_jul)
ENDIF
IF ( ASSOCIATED( grid%p_wif_aug ) ) THEN 
  DEALLOCATE(grid%p_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21069,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_aug. ')
 endif
  NULLIFY(grid%p_wif_aug)
ENDIF
IF ( ASSOCIATED( grid%p_wif_sep ) ) THEN 
  DEALLOCATE(grid%p_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21077,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_sep. ')
 endif
  NULLIFY(grid%p_wif_sep)
ENDIF
IF ( ASSOCIATED( grid%p_wif_oct ) ) THEN 
  DEALLOCATE(grid%p_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21085,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_oct. ')
 endif
  NULLIFY(grid%p_wif_oct)
ENDIF
IF ( ASSOCIATED( grid%p_wif_nov ) ) THEN 
  DEALLOCATE(grid%p_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21093,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_nov. ')
 endif
  NULLIFY(grid%p_wif_nov)
ENDIF
IF ( ASSOCIATED( grid%p_wif_dec ) ) THEN 
  DEALLOCATE(grid%p_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21101,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_dec. ')
 endif
  NULLIFY(grid%p_wif_dec)
ENDIF
IF ( ASSOCIATED( grid%w_wif_now ) ) THEN 
  DEALLOCATE(grid%w_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21109,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_now. ')
 endif
  NULLIFY(grid%w_wif_now)
ENDIF
IF ( ASSOCIATED( grid%w_wif_jan ) ) THEN 
  DEALLOCATE(grid%w_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21117,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_jan. ')
 endif
  NULLIFY(grid%w_wif_jan)
ENDIF
IF ( ASSOCIATED( grid%w_wif_feb ) ) THEN 
  DEALLOCATE(grid%w_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21125,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_feb. ')
 endif
  NULLIFY(grid%w_wif_feb)
ENDIF
IF ( ASSOCIATED( grid%w_wif_mar ) ) THEN 
  DEALLOCATE(grid%w_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21133,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_mar. ')
 endif
  NULLIFY(grid%w_wif_mar)
ENDIF
IF ( ASSOCIATED( grid%w_wif_apr ) ) THEN 
  DEALLOCATE(grid%w_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21141,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_apr. ')
 endif
  NULLIFY(grid%w_wif_apr)
ENDIF
IF ( ASSOCIATED( grid%w_wif_may ) ) THEN 
  DEALLOCATE(grid%w_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21149,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_may. ')
 endif
  NULLIFY(grid%w_wif_may)
ENDIF
IF ( ASSOCIATED( grid%w_wif_jun ) ) THEN 
  DEALLOCATE(grid%w_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21157,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_jun. ')
 endif
  NULLIFY(grid%w_wif_jun)
ENDIF
IF ( ASSOCIATED( grid%w_wif_jul ) ) THEN 
  DEALLOCATE(grid%w_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21165,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_jul. ')
 endif
  NULLIFY(grid%w_wif_jul)
ENDIF
IF ( ASSOCIATED( grid%w_wif_aug ) ) THEN 
  DEALLOCATE(grid%w_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21173,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_aug. ')
 endif
  NULLIFY(grid%w_wif_aug)
ENDIF
IF ( ASSOCIATED( grid%w_wif_sep ) ) THEN 
  DEALLOCATE(grid%w_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21181,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_sep. ')
 endif
  NULLIFY(grid%w_wif_sep)
ENDIF
IF ( ASSOCIATED( grid%w_wif_oct ) ) THEN 
  DEALLOCATE(grid%w_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21189,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_oct. ')
 endif
  NULLIFY(grid%w_wif_oct)
ENDIF
IF ( ASSOCIATED( grid%w_wif_nov ) ) THEN 
  DEALLOCATE(grid%w_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21197,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_nov. ')
 endif
  NULLIFY(grid%w_wif_nov)
ENDIF
IF ( ASSOCIATED( grid%w_wif_dec ) ) THEN 
  DEALLOCATE(grid%w_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21205,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_dec. ')
 endif
  NULLIFY(grid%w_wif_dec)
ENDIF
IF ( ASSOCIATED( grid%i_wif_now ) ) THEN 
  DEALLOCATE(grid%i_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21213,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_now. ')
 endif
  NULLIFY(grid%i_wif_now)
ENDIF
IF ( ASSOCIATED( grid%i_wif_jan ) ) THEN 
  DEALLOCATE(grid%i_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21221,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_jan. ')
 endif
  NULLIFY(grid%i_wif_jan)
ENDIF
IF ( ASSOCIATED( grid%i_wif_feb ) ) THEN 
  DEALLOCATE(grid%i_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21229,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_feb. ')
 endif
  NULLIFY(grid%i_wif_feb)
ENDIF
IF ( ASSOCIATED( grid%i_wif_mar ) ) THEN 
  DEALLOCATE(grid%i_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21237,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_mar. ')
 endif
  NULLIFY(grid%i_wif_mar)
ENDIF
IF ( ASSOCIATED( grid%i_wif_apr ) ) THEN 
  DEALLOCATE(grid%i_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21245,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_apr. ')
 endif
  NULLIFY(grid%i_wif_apr)
ENDIF
IF ( ASSOCIATED( grid%i_wif_may ) ) THEN 
  DEALLOCATE(grid%i_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21253,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_may. ')
 endif
  NULLIFY(grid%i_wif_may)
ENDIF
IF ( ASSOCIATED( grid%i_wif_jun ) ) THEN 
  DEALLOCATE(grid%i_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21261,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_jun. ')
 endif
  NULLIFY(grid%i_wif_jun)
ENDIF
IF ( ASSOCIATED( grid%i_wif_jul ) ) THEN 
  DEALLOCATE(grid%i_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21269,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_jul. ')
 endif
  NULLIFY(grid%i_wif_jul)
ENDIF
IF ( ASSOCIATED( grid%i_wif_aug ) ) THEN 
  DEALLOCATE(grid%i_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21277,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_aug. ')
 endif
  NULLIFY(grid%i_wif_aug)
ENDIF
IF ( ASSOCIATED( grid%i_wif_sep ) ) THEN 
  DEALLOCATE(grid%i_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21285,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_sep. ')
 endif
  NULLIFY(grid%i_wif_sep)
ENDIF
IF ( ASSOCIATED( grid%i_wif_oct ) ) THEN 
  DEALLOCATE(grid%i_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21293,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_oct. ')
 endif
  NULLIFY(grid%i_wif_oct)
ENDIF
IF ( ASSOCIATED( grid%i_wif_nov ) ) THEN 
  DEALLOCATE(grid%i_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21301,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_nov. ')
 endif
  NULLIFY(grid%i_wif_nov)
ENDIF
IF ( ASSOCIATED( grid%i_wif_dec ) ) THEN 
  DEALLOCATE(grid%i_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21309,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_dec. ')
 endif
  NULLIFY(grid%i_wif_dec)
ENDIF
IF ( ASSOCIATED( grid%b_wif_now ) ) THEN 
  DEALLOCATE(grid%b_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21317,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_now. ')
 endif
  NULLIFY(grid%b_wif_now)
ENDIF
IF ( ASSOCIATED( grid%b_wif_jan ) ) THEN 
  DEALLOCATE(grid%b_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21325,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_jan. ')
 endif
  NULLIFY(grid%b_wif_jan)
ENDIF
IF ( ASSOCIATED( grid%b_wif_feb ) ) THEN 
  DEALLOCATE(grid%b_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21333,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_feb. ')
 endif
  NULLIFY(grid%b_wif_feb)
ENDIF
IF ( ASSOCIATED( grid%b_wif_mar ) ) THEN 
  DEALLOCATE(grid%b_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21341,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_mar. ')
 endif
  NULLIFY(grid%b_wif_mar)
ENDIF
IF ( ASSOCIATED( grid%b_wif_apr ) ) THEN 
  DEALLOCATE(grid%b_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21349,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_apr. ')
 endif
  NULLIFY(grid%b_wif_apr)
ENDIF
IF ( ASSOCIATED( grid%b_wif_may ) ) THEN 
  DEALLOCATE(grid%b_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21357,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_may. ')
 endif
  NULLIFY(grid%b_wif_may)
ENDIF
IF ( ASSOCIATED( grid%b_wif_jun ) ) THEN 
  DEALLOCATE(grid%b_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21365,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_jun. ')
 endif
  NULLIFY(grid%b_wif_jun)
ENDIF
IF ( ASSOCIATED( grid%b_wif_jul ) ) THEN 
  DEALLOCATE(grid%b_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21373,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_jul. ')
 endif
  NULLIFY(grid%b_wif_jul)
ENDIF
IF ( ASSOCIATED( grid%b_wif_aug ) ) THEN 
  DEALLOCATE(grid%b_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21381,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_aug. ')
 endif
  NULLIFY(grid%b_wif_aug)
ENDIF
IF ( ASSOCIATED( grid%b_wif_sep ) ) THEN 
  DEALLOCATE(grid%b_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21389,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_sep. ')
 endif
  NULLIFY(grid%b_wif_sep)
ENDIF
IF ( ASSOCIATED( grid%b_wif_oct ) ) THEN 
  DEALLOCATE(grid%b_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21397,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_oct. ')
 endif
  NULLIFY(grid%b_wif_oct)
ENDIF
IF ( ASSOCIATED( grid%b_wif_nov ) ) THEN 
  DEALLOCATE(grid%b_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21405,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_nov. ')
 endif
  NULLIFY(grid%b_wif_nov)
ENDIF
IF ( ASSOCIATED( grid%b_wif_dec ) ) THEN 
  DEALLOCATE(grid%b_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21413,&
'frame/module_domain.f: Failed to deallocate grid%b_wif_dec. ')
 endif
  NULLIFY(grid%b_wif_dec)
ENDIF
IF ( ASSOCIATED( grid%sealevelp ) ) THEN 
  DEALLOCATE(grid%sealevelp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21421,&
'frame/module_domain.f: Failed to deallocate grid%sealevelp. ')
 endif
  NULLIFY(grid%sealevelp)
ENDIF
IF ( ASSOCIATED( grid%temperature ) ) THEN 
  DEALLOCATE(grid%temperature,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21429,&
'frame/module_domain.f: Failed to deallocate grid%temperature. ')
 endif
  NULLIFY(grid%temperature)
ENDIF
IF ( ASSOCIATED( grid%geoheight ) ) THEN 
  DEALLOCATE(grid%geoheight,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21437,&
'frame/module_domain.f: Failed to deallocate grid%geoheight. ')
 endif
  NULLIFY(grid%geoheight)
ENDIF
IF ( ASSOCIATED( grid%pressure ) ) THEN 
  DEALLOCATE(grid%pressure,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21445,&
'frame/module_domain.f: Failed to deallocate grid%pressure. ')
 endif
  NULLIFY(grid%pressure)
ENDIF
IF ( ASSOCIATED( grid%umet ) ) THEN 
  DEALLOCATE(grid%umet,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21453,&
'frame/module_domain.f: Failed to deallocate grid%umet. ')
 endif
  NULLIFY(grid%umet)
ENDIF
IF ( ASSOCIATED( grid%vmet ) ) THEN 
  DEALLOCATE(grid%vmet,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21461,&
'frame/module_domain.f: Failed to deallocate grid%vmet. ')
 endif
  NULLIFY(grid%vmet)
ENDIF
IF ( ASSOCIATED( grid%speed ) ) THEN 
  DEALLOCATE(grid%speed,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21469,&
'frame/module_domain.f: Failed to deallocate grid%speed. ')
 endif
  NULLIFY(grid%speed)
ENDIF
IF ( ASSOCIATED( grid%dir ) ) THEN 
  DEALLOCATE(grid%dir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21477,&
'frame/module_domain.f: Failed to deallocate grid%dir. ')
 endif
  NULLIFY(grid%dir)
ENDIF
IF ( ASSOCIATED( grid%rain ) ) THEN 
  DEALLOCATE(grid%rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21485,&
'frame/module_domain.f: Failed to deallocate grid%rain. ')
 endif
  NULLIFY(grid%rain)
ENDIF
IF ( ASSOCIATED( grid%liqrain ) ) THEN 
  DEALLOCATE(grid%liqrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21493,&
'frame/module_domain.f: Failed to deallocate grid%liqrain. ')
 endif
  NULLIFY(grid%liqrain)
ENDIF
IF ( ASSOCIATED( grid%tpw ) ) THEN 
  DEALLOCATE(grid%tpw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21501,&
'frame/module_domain.f: Failed to deallocate grid%tpw. ')
 endif
  NULLIFY(grid%tpw)
ENDIF
IF ( ASSOCIATED( grid%potential_t ) ) THEN 
  DEALLOCATE(grid%potential_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21509,&
'frame/module_domain.f: Failed to deallocate grid%potential_t. ')
 endif
  NULLIFY(grid%potential_t)
ENDIF
IF ( ASSOCIATED( grid%rh ) ) THEN 
  DEALLOCATE(grid%rh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21517,&
'frame/module_domain.f: Failed to deallocate grid%rh. ')
 endif
  NULLIFY(grid%rh)
ENDIF
IF ( ASSOCIATED( grid%qc_tot ) ) THEN 
  DEALLOCATE(grid%qc_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21525,&
'frame/module_domain.f: Failed to deallocate grid%qc_tot. ')
 endif
  NULLIFY(grid%qc_tot)
ENDIF
IF ( ASSOCIATED( grid%qi_tot ) ) THEN 
  DEALLOCATE(grid%qi_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21533,&
'frame/module_domain.f: Failed to deallocate grid%qi_tot. ')
 endif
  NULLIFY(grid%qi_tot)
ENDIF
IF ( ASSOCIATED( grid%cldfrac2d ) ) THEN 
  DEALLOCATE(grid%cldfrac2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21541,&
'frame/module_domain.f: Failed to deallocate grid%cldfrac2d. ')
 endif
  NULLIFY(grid%cldfrac2d)
ENDIF
IF ( ASSOCIATED( grid%wvp ) ) THEN 
  DEALLOCATE(grid%wvp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21549,&
'frame/module_domain.f: Failed to deallocate grid%wvp. ')
 endif
  NULLIFY(grid%wvp)
ENDIF
IF ( ASSOCIATED( grid%lwp ) ) THEN 
  DEALLOCATE(grid%lwp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21557,&
'frame/module_domain.f: Failed to deallocate grid%lwp. ')
 endif
  NULLIFY(grid%lwp)
ENDIF
IF ( ASSOCIATED( grid%iwp ) ) THEN 
  DEALLOCATE(grid%iwp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21565,&
'frame/module_domain.f: Failed to deallocate grid%iwp. ')
 endif
  NULLIFY(grid%iwp)
ENDIF
IF ( ASSOCIATED( grid%swp ) ) THEN 
  DEALLOCATE(grid%swp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21573,&
'frame/module_domain.f: Failed to deallocate grid%swp. ')
 endif
  NULLIFY(grid%swp)
ENDIF
IF ( ASSOCIATED( grid%wp_sum ) ) THEN 
  DEALLOCATE(grid%wp_sum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21581,&
'frame/module_domain.f: Failed to deallocate grid%wp_sum. ')
 endif
  NULLIFY(grid%wp_sum)
ENDIF
IF ( ASSOCIATED( grid%lwp_tot ) ) THEN 
  DEALLOCATE(grid%lwp_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21589,&
'frame/module_domain.f: Failed to deallocate grid%lwp_tot. ')
 endif
  NULLIFY(grid%lwp_tot)
ENDIF
IF ( ASSOCIATED( grid%iwp_tot ) ) THEN 
  DEALLOCATE(grid%iwp_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21597,&
'frame/module_domain.f: Failed to deallocate grid%iwp_tot. ')
 endif
  NULLIFY(grid%iwp_tot)
ENDIF
IF ( ASSOCIATED( grid%wp_tot_sum ) ) THEN 
  DEALLOCATE(grid%wp_tot_sum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21605,&
'frame/module_domain.f: Failed to deallocate grid%wp_tot_sum. ')
 endif
  NULLIFY(grid%wp_tot_sum)
ENDIF
IF ( ASSOCIATED( grid%re_qc ) ) THEN 
  DEALLOCATE(grid%re_qc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21613,&
'frame/module_domain.f: Failed to deallocate grid%re_qc. ')
 endif
  NULLIFY(grid%re_qc)
ENDIF
IF ( ASSOCIATED( grid%re_qi ) ) THEN 
  DEALLOCATE(grid%re_qi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21621,&
'frame/module_domain.f: Failed to deallocate grid%re_qi. ')
 endif
  NULLIFY(grid%re_qi)
ENDIF
IF ( ASSOCIATED( grid%re_qs ) ) THEN 
  DEALLOCATE(grid%re_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21629,&
'frame/module_domain.f: Failed to deallocate grid%re_qs. ')
 endif
  NULLIFY(grid%re_qs)
ENDIF
IF ( ASSOCIATED( grid%re_qc_tot ) ) THEN 
  DEALLOCATE(grid%re_qc_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21637,&
'frame/module_domain.f: Failed to deallocate grid%re_qc_tot. ')
 endif
  NULLIFY(grid%re_qc_tot)
ENDIF
IF ( ASSOCIATED( grid%re_qi_tot ) ) THEN 
  DEALLOCATE(grid%re_qi_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21645,&
'frame/module_domain.f: Failed to deallocate grid%re_qi_tot. ')
 endif
  NULLIFY(grid%re_qi_tot)
ENDIF
IF ( ASSOCIATED( grid%tau_qc ) ) THEN 
  DEALLOCATE(grid%tau_qc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21653,&
'frame/module_domain.f: Failed to deallocate grid%tau_qc. ')
 endif
  NULLIFY(grid%tau_qc)
ENDIF
IF ( ASSOCIATED( grid%tau_qi ) ) THEN 
  DEALLOCATE(grid%tau_qi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21661,&
'frame/module_domain.f: Failed to deallocate grid%tau_qi. ')
 endif
  NULLIFY(grid%tau_qi)
ENDIF
IF ( ASSOCIATED( grid%tau_qs ) ) THEN 
  DEALLOCATE(grid%tau_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21669,&
'frame/module_domain.f: Failed to deallocate grid%tau_qs. ')
 endif
  NULLIFY(grid%tau_qs)
ENDIF
IF ( ASSOCIATED( grid%tau_qc_tot ) ) THEN 
  DEALLOCATE(grid%tau_qc_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21677,&
'frame/module_domain.f: Failed to deallocate grid%tau_qc_tot. ')
 endif
  NULLIFY(grid%tau_qc_tot)
ENDIF
IF ( ASSOCIATED( grid%tau_qi_tot ) ) THEN 
  DEALLOCATE(grid%tau_qi_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21685,&
'frame/module_domain.f: Failed to deallocate grid%tau_qi_tot. ')
 endif
  NULLIFY(grid%tau_qi_tot)
ENDIF
IF ( ASSOCIATED( grid%cbaseht ) ) THEN 
  DEALLOCATE(grid%cbaseht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21693,&
'frame/module_domain.f: Failed to deallocate grid%cbaseht. ')
 endif
  NULLIFY(grid%cbaseht)
ENDIF
IF ( ASSOCIATED( grid%ctopht ) ) THEN 
  DEALLOCATE(grid%ctopht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21701,&
'frame/module_domain.f: Failed to deallocate grid%ctopht. ')
 endif
  NULLIFY(grid%ctopht)
ENDIF
IF ( ASSOCIATED( grid%cbaseht_tot ) ) THEN 
  DEALLOCATE(grid%cbaseht_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21709,&
'frame/module_domain.f: Failed to deallocate grid%cbaseht_tot. ')
 endif
  NULLIFY(grid%cbaseht_tot)
ENDIF
IF ( ASSOCIATED( grid%ctopht_tot ) ) THEN 
  DEALLOCATE(grid%ctopht_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21717,&
'frame/module_domain.f: Failed to deallocate grid%ctopht_tot. ')
 endif
  NULLIFY(grid%ctopht_tot)
ENDIF
IF ( ASSOCIATED( grid%clrnidx ) ) THEN 
  DEALLOCATE(grid%clrnidx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21725,&
'frame/module_domain.f: Failed to deallocate grid%clrnidx. ')
 endif
  NULLIFY(grid%clrnidx)
ENDIF
IF ( ASSOCIATED( grid%sza ) ) THEN 
  DEALLOCATE(grid%sza,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21733,&
'frame/module_domain.f: Failed to deallocate grid%sza. ')
 endif
  NULLIFY(grid%sza)
ENDIF
IF ( ASSOCIATED( grid%ghi_accum ) ) THEN 
  DEALLOCATE(grid%ghi_accum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21741,&
'frame/module_domain.f: Failed to deallocate grid%ghi_accum. ')
 endif
  NULLIFY(grid%ghi_accum)
ENDIF
IF ( ASSOCIATED( grid%ts_cldfrac2d ) ) THEN 
  DEALLOCATE(grid%ts_cldfrac2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21749,&
'frame/module_domain.f: Failed to deallocate grid%ts_cldfrac2d. ')
 endif
  NULLIFY(grid%ts_cldfrac2d)
ENDIF
IF ( ASSOCIATED( grid%ts_wvp ) ) THEN 
  DEALLOCATE(grid%ts_wvp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21757,&
'frame/module_domain.f: Failed to deallocate grid%ts_wvp. ')
 endif
  NULLIFY(grid%ts_wvp)
ENDIF
IF ( ASSOCIATED( grid%ts_lwp ) ) THEN 
  DEALLOCATE(grid%ts_lwp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21765,&
'frame/module_domain.f: Failed to deallocate grid%ts_lwp. ')
 endif
  NULLIFY(grid%ts_lwp)
ENDIF
IF ( ASSOCIATED( grid%ts_iwp ) ) THEN 
  DEALLOCATE(grid%ts_iwp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21773,&
'frame/module_domain.f: Failed to deallocate grid%ts_iwp. ')
 endif
  NULLIFY(grid%ts_iwp)
ENDIF
IF ( ASSOCIATED( grid%ts_swp ) ) THEN 
  DEALLOCATE(grid%ts_swp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21781,&
'frame/module_domain.f: Failed to deallocate grid%ts_swp. ')
 endif
  NULLIFY(grid%ts_swp)
ENDIF
IF ( ASSOCIATED( grid%ts_wp_sum ) ) THEN 
  DEALLOCATE(grid%ts_wp_sum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21789,&
'frame/module_domain.f: Failed to deallocate grid%ts_wp_sum. ')
 endif
  NULLIFY(grid%ts_wp_sum)
ENDIF
IF ( ASSOCIATED( grid%ts_lwp_tot ) ) THEN 
  DEALLOCATE(grid%ts_lwp_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21797,&
'frame/module_domain.f: Failed to deallocate grid%ts_lwp_tot. ')
 endif
  NULLIFY(grid%ts_lwp_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_iwp_tot ) ) THEN 
  DEALLOCATE(grid%ts_iwp_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21805,&
'frame/module_domain.f: Failed to deallocate grid%ts_iwp_tot. ')
 endif
  NULLIFY(grid%ts_iwp_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_wp_tot_sum ) ) THEN 
  DEALLOCATE(grid%ts_wp_tot_sum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21813,&
'frame/module_domain.f: Failed to deallocate grid%ts_wp_tot_sum. ')
 endif
  NULLIFY(grid%ts_wp_tot_sum)
ENDIF
IF ( ASSOCIATED( grid%ts_re_qc ) ) THEN 
  DEALLOCATE(grid%ts_re_qc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21821,&
'frame/module_domain.f: Failed to deallocate grid%ts_re_qc. ')
 endif
  NULLIFY(grid%ts_re_qc)
ENDIF
IF ( ASSOCIATED( grid%ts_re_qi ) ) THEN 
  DEALLOCATE(grid%ts_re_qi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21829,&
'frame/module_domain.f: Failed to deallocate grid%ts_re_qi. ')
 endif
  NULLIFY(grid%ts_re_qi)
ENDIF
IF ( ASSOCIATED( grid%ts_re_qs ) ) THEN 
  DEALLOCATE(grid%ts_re_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21837,&
'frame/module_domain.f: Failed to deallocate grid%ts_re_qs. ')
 endif
  NULLIFY(grid%ts_re_qs)
ENDIF
IF ( ASSOCIATED( grid%ts_re_qc_tot ) ) THEN 
  DEALLOCATE(grid%ts_re_qc_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21845,&
'frame/module_domain.f: Failed to deallocate grid%ts_re_qc_tot. ')
 endif
  NULLIFY(grid%ts_re_qc_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_re_qi_tot ) ) THEN 
  DEALLOCATE(grid%ts_re_qi_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21853,&
'frame/module_domain.f: Failed to deallocate grid%ts_re_qi_tot. ')
 endif
  NULLIFY(grid%ts_re_qi_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_tau_qc ) ) THEN 
  DEALLOCATE(grid%ts_tau_qc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21861,&
'frame/module_domain.f: Failed to deallocate grid%ts_tau_qc. ')
 endif
  NULLIFY(grid%ts_tau_qc)
ENDIF
IF ( ASSOCIATED( grid%ts_tau_qi ) ) THEN 
  DEALLOCATE(grid%ts_tau_qi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21869,&
'frame/module_domain.f: Failed to deallocate grid%ts_tau_qi. ')
 endif
  NULLIFY(grid%ts_tau_qi)
ENDIF
IF ( ASSOCIATED( grid%ts_tau_qs ) ) THEN 
  DEALLOCATE(grid%ts_tau_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21877,&
'frame/module_domain.f: Failed to deallocate grid%ts_tau_qs. ')
 endif
  NULLIFY(grid%ts_tau_qs)
ENDIF
IF ( ASSOCIATED( grid%ts_tau_qc_tot ) ) THEN 
  DEALLOCATE(grid%ts_tau_qc_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21885,&
'frame/module_domain.f: Failed to deallocate grid%ts_tau_qc_tot. ')
 endif
  NULLIFY(grid%ts_tau_qc_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_tau_qi_tot ) ) THEN 
  DEALLOCATE(grid%ts_tau_qi_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21893,&
'frame/module_domain.f: Failed to deallocate grid%ts_tau_qi_tot. ')
 endif
  NULLIFY(grid%ts_tau_qi_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_cbaseht ) ) THEN 
  DEALLOCATE(grid%ts_cbaseht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21901,&
'frame/module_domain.f: Failed to deallocate grid%ts_cbaseht. ')
 endif
  NULLIFY(grid%ts_cbaseht)
ENDIF
IF ( ASSOCIATED( grid%ts_ctopht ) ) THEN 
  DEALLOCATE(grid%ts_ctopht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21909,&
'frame/module_domain.f: Failed to deallocate grid%ts_ctopht. ')
 endif
  NULLIFY(grid%ts_ctopht)
ENDIF
IF ( ASSOCIATED( grid%ts_cbaseht_tot ) ) THEN 
  DEALLOCATE(grid%ts_cbaseht_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21917,&
'frame/module_domain.f: Failed to deallocate grid%ts_cbaseht_tot. ')
 endif
  NULLIFY(grid%ts_cbaseht_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_ctopht_tot ) ) THEN 
  DEALLOCATE(grid%ts_ctopht_tot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21925,&
'frame/module_domain.f: Failed to deallocate grid%ts_ctopht_tot. ')
 endif
  NULLIFY(grid%ts_ctopht_tot)
ENDIF
IF ( ASSOCIATED( grid%ts_clrnidx ) ) THEN 
  DEALLOCATE(grid%ts_clrnidx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21933,&
'frame/module_domain.f: Failed to deallocate grid%ts_clrnidx. ')
 endif
  NULLIFY(grid%ts_clrnidx)
ENDIF
IF ( ASSOCIATED( grid%ts_sza ) ) THEN 
  DEALLOCATE(grid%ts_sza,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21941,&
'frame/module_domain.f: Failed to deallocate grid%ts_sza. ')
 endif
  NULLIFY(grid%ts_sza)
ENDIF
IF ( ASSOCIATED( grid%ts_ghi_accum ) ) THEN 
  DEALLOCATE(grid%ts_ghi_accum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21949,&
'frame/module_domain.f: Failed to deallocate grid%ts_ghi_accum. ')
 endif
  NULLIFY(grid%ts_ghi_accum)
ENDIF
IF ( ASSOCIATED( grid%ts_swdown ) ) THEN 
  DEALLOCATE(grid%ts_swdown,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21957,&
'frame/module_domain.f: Failed to deallocate grid%ts_swdown. ')
 endif
  NULLIFY(grid%ts_swdown)
ENDIF
IF ( ASSOCIATED( grid%ts_swddni ) ) THEN 
  DEALLOCATE(grid%ts_swddni,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21965,&
'frame/module_domain.f: Failed to deallocate grid%ts_swddni. ')
 endif
  NULLIFY(grid%ts_swddni)
ENDIF
IF ( ASSOCIATED( grid%ts_swddif ) ) THEN 
  DEALLOCATE(grid%ts_swddif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21973,&
'frame/module_domain.f: Failed to deallocate grid%ts_swddif. ')
 endif
  NULLIFY(grid%ts_swddif)
ENDIF
IF ( ASSOCIATED( grid%ts_swdownc ) ) THEN 
  DEALLOCATE(grid%ts_swdownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21981,&
'frame/module_domain.f: Failed to deallocate grid%ts_swdownc. ')
 endif
  NULLIFY(grid%ts_swdownc)
ENDIF
IF ( ASSOCIATED( grid%ts_swddnic ) ) THEN 
  DEALLOCATE(grid%ts_swddnic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21989,&
'frame/module_domain.f: Failed to deallocate grid%ts_swddnic. ')
 endif
  NULLIFY(grid%ts_swddnic)
ENDIF
IF ( ASSOCIATED( grid%ts_swdown2 ) ) THEN 
  DEALLOCATE(grid%ts_swdown2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",21997,&
'frame/module_domain.f: Failed to deallocate grid%ts_swdown2. ')
 endif
  NULLIFY(grid%ts_swdown2)
ENDIF
IF ( ASSOCIATED( grid%ts_swddni2 ) ) THEN 
  DEALLOCATE(grid%ts_swddni2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22005,&
'frame/module_domain.f: Failed to deallocate grid%ts_swddni2. ')
 endif
  NULLIFY(grid%ts_swddni2)
ENDIF
IF ( ASSOCIATED( grid%ts_swddif2 ) ) THEN 
  DEALLOCATE(grid%ts_swddif2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22013,&
'frame/module_domain.f: Failed to deallocate grid%ts_swddif2. ')
 endif
  NULLIFY(grid%ts_swddif2)
ENDIF
IF ( ASSOCIATED( grid%ts_swdownc2 ) ) THEN 
  DEALLOCATE(grid%ts_swdownc2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22021,&
'frame/module_domain.f: Failed to deallocate grid%ts_swdownc2. ')
 endif
  NULLIFY(grid%ts_swdownc2)
ENDIF
IF ( ASSOCIATED( grid%ts_swddnic2 ) ) THEN 
  DEALLOCATE(grid%ts_swddnic2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22029,&
'frame/module_domain.f: Failed to deallocate grid%ts_swddnic2. ')
 endif
  NULLIFY(grid%ts_swddnic2)
ENDIF
IF ( ASSOCIATED( grid%p_pl ) ) THEN 
  DEALLOCATE(grid%p_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22037,&
'frame/module_domain.f: Failed to deallocate grid%p_pl. ')
 endif
  NULLIFY(grid%p_pl)
ENDIF
IF ( ASSOCIATED( grid%u_pl ) ) THEN 
  DEALLOCATE(grid%u_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22045,&
'frame/module_domain.f: Failed to deallocate grid%u_pl. ')
 endif
  NULLIFY(grid%u_pl)
ENDIF
IF ( ASSOCIATED( grid%v_pl ) ) THEN 
  DEALLOCATE(grid%v_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22053,&
'frame/module_domain.f: Failed to deallocate grid%v_pl. ')
 endif
  NULLIFY(grid%v_pl)
ENDIF
IF ( ASSOCIATED( grid%t_pl ) ) THEN 
  DEALLOCATE(grid%t_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22061,&
'frame/module_domain.f: Failed to deallocate grid%t_pl. ')
 endif
  NULLIFY(grid%t_pl)
ENDIF
IF ( ASSOCIATED( grid%rh_pl ) ) THEN 
  DEALLOCATE(grid%rh_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22069,&
'frame/module_domain.f: Failed to deallocate grid%rh_pl. ')
 endif
  NULLIFY(grid%rh_pl)
ENDIF
IF ( ASSOCIATED( grid%ght_pl ) ) THEN 
  DEALLOCATE(grid%ght_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22077,&
'frame/module_domain.f: Failed to deallocate grid%ght_pl. ')
 endif
  NULLIFY(grid%ght_pl)
ENDIF
IF ( ASSOCIATED( grid%s_pl ) ) THEN 
  DEALLOCATE(grid%s_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22085,&
'frame/module_domain.f: Failed to deallocate grid%s_pl. ')
 endif
  NULLIFY(grid%s_pl)
ENDIF
IF ( ASSOCIATED( grid%td_pl ) ) THEN 
  DEALLOCATE(grid%td_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22093,&
'frame/module_domain.f: Failed to deallocate grid%td_pl. ')
 endif
  NULLIFY(grid%td_pl)
ENDIF
IF ( ASSOCIATED( grid%q_pl ) ) THEN 
  DEALLOCATE(grid%q_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22101,&
'frame/module_domain.f: Failed to deallocate grid%q_pl. ')
 endif
  NULLIFY(grid%q_pl)
ENDIF
IF ( ASSOCIATED( grid%z_zl ) ) THEN 
  DEALLOCATE(grid%z_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22109,&
'frame/module_domain.f: Failed to deallocate grid%z_zl. ')
 endif
  NULLIFY(grid%z_zl)
ENDIF
IF ( ASSOCIATED( grid%u_zl ) ) THEN 
  DEALLOCATE(grid%u_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22117,&
'frame/module_domain.f: Failed to deallocate grid%u_zl. ')
 endif
  NULLIFY(grid%u_zl)
ENDIF
IF ( ASSOCIATED( grid%v_zl ) ) THEN 
  DEALLOCATE(grid%v_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22125,&
'frame/module_domain.f: Failed to deallocate grid%v_zl. ')
 endif
  NULLIFY(grid%v_zl)
ENDIF
IF ( ASSOCIATED( grid%t_zl ) ) THEN 
  DEALLOCATE(grid%t_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22133,&
'frame/module_domain.f: Failed to deallocate grid%t_zl. ')
 endif
  NULLIFY(grid%t_zl)
ENDIF
IF ( ASSOCIATED( grid%rh_zl ) ) THEN 
  DEALLOCATE(grid%rh_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22141,&
'frame/module_domain.f: Failed to deallocate grid%rh_zl. ')
 endif
  NULLIFY(grid%rh_zl)
ENDIF
IF ( ASSOCIATED( grid%ght_zl ) ) THEN 
  DEALLOCATE(grid%ght_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22149,&
'frame/module_domain.f: Failed to deallocate grid%ght_zl. ')
 endif
  NULLIFY(grid%ght_zl)
ENDIF
IF ( ASSOCIATED( grid%s_zl ) ) THEN 
  DEALLOCATE(grid%s_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22157,&
'frame/module_domain.f: Failed to deallocate grid%s_zl. ')
 endif
  NULLIFY(grid%s_zl)
ENDIF
IF ( ASSOCIATED( grid%td_zl ) ) THEN 
  DEALLOCATE(grid%td_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22165,&
'frame/module_domain.f: Failed to deallocate grid%td_zl. ')
 endif
  NULLIFY(grid%td_zl)
ENDIF
IF ( ASSOCIATED( grid%q_zl ) ) THEN 
  DEALLOCATE(grid%q_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22173,&
'frame/module_domain.f: Failed to deallocate grid%q_zl. ')
 endif
  NULLIFY(grid%q_zl)
ENDIF
IF ( ASSOCIATED( grid%p_zl ) ) THEN 
  DEALLOCATE(grid%p_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22181,&
'frame/module_domain.f: Failed to deallocate grid%p_zl. ')
 endif
  NULLIFY(grid%p_zl)
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22189,&
'frame/module_domain.f: Failed to deallocate grid%landmask. ')
 endif
  NULLIFY(grid%landmask)
ENDIF
IF ( ASSOCIATED( grid%lakemask ) ) THEN 
  DEALLOCATE(grid%lakemask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22197,&
'frame/module_domain.f: Failed to deallocate grid%lakemask. ')
 endif
  NULLIFY(grid%lakemask)
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22205,&
'frame/module_domain.f: Failed to deallocate grid%sst. ')
 endif
  NULLIFY(grid%sst)
ENDIF
IF ( ASSOCIATED( grid%sst_input ) ) THEN 
  DEALLOCATE(grid%sst_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22213,&
'frame/module_domain.f: Failed to deallocate grid%sst_input. ')
 endif
  NULLIFY(grid%sst_input)
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",22221,&
'frame/module_domain.f: Failed to deallocate grid%chem. ')
 endif
  NULLIFY(grid%chem)
ENDIF


   END SUBROUTINE dealloc_space_field



   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid






      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      NULLIFY(result_grid)
      IF ( ASSOCIATED( in_grid ) ) THEN
        IF ( in_grid%id .EQ. id ) THEN
           result_grid => in_grid
        ELSE
           grid_ptr => in_grid
           DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
              DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                    CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                    IF ( ASSOCIATED( result_grid ) ) THEN
                      IF ( result_grid%id .EQ. id ) found = .TRUE.
                    ENDIF
                 ENDIF
              ENDDO
              IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
           ENDDO
        ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      

      INTEGER                             :: loc






      
      

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer

   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain










      FUNCTION domain_get_current_time ( grid ) RESULT ( current_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: current_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, CurrTime=current_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22329,&
            'domain_get_current_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_current_time


      FUNCTION domain_get_start_time ( grid ) RESULT ( start_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: start_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StartTime=start_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22349,&
            'domain_get_start_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_start_time


      FUNCTION domain_get_stop_time ( grid ) RESULT ( stop_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: stop_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StopTime=stop_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22369,&
            'domain_get_stop_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_stop_time


      FUNCTION domain_get_time_step ( grid ) RESULT ( time_step ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_step
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, timeStep=time_step, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22389,&
            'domain_get_time_step:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_time_step


      FUNCTION domain_get_advanceCount ( grid ) RESULT ( advanceCount ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        INTEGER :: advanceCount
        
        INTEGER(WRFU_KIND_I8) :: advanceCountLcl
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, &
                            advanceCount=advanceCountLcl, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22412,&
            'domain_get_advanceCount:  WRFU_ClockGet failed' )
        ENDIF
        advanceCount = advanceCountLcl
      END FUNCTION domain_get_advanceCount


      SUBROUTINE domain_alarms_destroy ( grid )
        IMPLICIT NONE





        TYPE(domain), INTENT(INOUT) :: grid
        
        INTEGER                     :: alarmid

        IF ( ASSOCIATED( grid%alarms ) .AND. &
             ASSOCIATED( grid%alarms_created ) ) THEN
          DO alarmid = 1, MAX_WRF_ALARMS
            IF ( grid%alarms_created( alarmid ) ) THEN
              CALL WRFU_AlarmDestroy( grid%alarms( alarmid ) )
              grid%alarms_created( alarmid ) = .FALSE.
            ENDIF
          ENDDO
          DEALLOCATE( grid%alarms )
          NULLIFY( grid%alarms )
          DEALLOCATE( grid%alarms_created )
          NULLIFY( grid%alarms_created )
        ENDIF
      END SUBROUTINE domain_alarms_destroy


      SUBROUTINE domain_clock_destroy ( grid )
        IMPLICIT NONE




        TYPE(domain), INTENT(INOUT) :: grid
        IF ( ASSOCIATED( grid%domain_clock ) ) THEN
          IF ( grid%domain_clock_created ) THEN
            CALL WRFU_ClockDestroy( grid%domain_clock )
            grid%domain_clock_created = .FALSE.
          ENDIF
          DEALLOCATE( grid%domain_clock )
          NULLIFY( grid%domain_clock )
        ENDIF
      END SUBROUTINE domain_clock_destroy


      FUNCTION domain_last_time_step ( grid ) RESULT ( LAST_TIME ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: LAST_TIME
        LAST_TIME =   domain_get_stop_time( grid ) .EQ. &
                    ( domain_get_current_time( grid ) + &
                      domain_get_time_step( grid ) )
      END FUNCTION domain_last_time_step



      FUNCTION domain_clockisstoptime ( grid ) RESULT ( is_stop_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_time
        INTEGER :: rc
        is_stop_time = WRFU_ClockIsStopTime( grid%domain_clock , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22494,&
            'domain_clockisstoptime:  WRFU_ClockIsStopTime() failed' )
        ENDIF
      END FUNCTION domain_clockisstoptime



      FUNCTION domain_clockisstopsubtime ( grid ) RESULT ( is_stop_subtime ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_subtime
        INTEGER :: rc
        TYPE(WRFU_TimeInterval) :: timeStep
        TYPE(WRFU_Time) :: currentTime
        LOGICAL :: positive_timestep
        is_stop_subtime = .FALSE.
        CALL domain_clock_get( grid, time_step=timeStep, &
                                     current_time=currentTime )
        positive_timestep = ESMF_TimeIntervalIsPositive( timeStep )
        IF ( positive_timestep ) THEN


          IF ( ESMF_TimeGE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ELSE


          IF ( ESMF_TimeLE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ENDIF
      END FUNCTION domain_clockisstopsubtime




      FUNCTION domain_get_sim_start_time ( grid ) RESULT ( simulationStartTime ) 
        IMPLICIT NONE












        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: simulationStartTime
        
        INTEGER :: rc
        INTEGER :: simulation_start_year,   simulation_start_month, &
                   simulation_start_day,    simulation_start_hour , &
                   simulation_start_minute, simulation_start_second
        CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
        CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
        CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
        CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
        CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
        CALL nl_get_simulation_start_second ( 1, simulation_start_second )
        CALL WRFU_TimeSet( simulationStartTime,       &
                           YY=simulation_start_year,  &
                           MM=simulation_start_month, &
                           DD=simulation_start_day,   &
                           H=simulation_start_hour,   &
                           M=simulation_start_minute, &
                           S=simulation_start_second, &
                           rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL nl_get_start_year   ( 1, simulation_start_year   )
          CALL nl_get_start_month  ( 1, simulation_start_month  )
          CALL nl_get_start_day    ( 1, simulation_start_day    )
          CALL nl_get_start_hour   ( 1, simulation_start_hour   )
          CALL nl_get_start_minute ( 1, simulation_start_minute )
          CALL nl_get_start_second ( 1, simulation_start_second )
          CALL wrf_debug( 150, "WARNING:  domain_get_sim_start_time using head_grid start time from namelist" )
          CALL WRFU_TimeSet( simulationStartTime,       &
                             YY=simulation_start_year,  &
                             MM=simulation_start_month, &
                             DD=simulation_start_day,   &
                             H=simulation_start_hour,   &
                             M=simulation_start_minute, &
                             S=simulation_start_second, &
                             rc=rc )
        ENDIF
        RETURN
      END FUNCTION domain_get_sim_start_time

      FUNCTION domain_get_time_since_sim_start ( grid ) RESULT ( time_since_sim_start ) 
        IMPLICIT NONE









        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_since_sim_start
        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_simstarttime
        lcl_simstarttime = domain_get_sim_start_time( grid )
        lcl_currtime = domain_get_current_time ( grid )
        time_since_sim_start = lcl_currtime - lcl_simstarttime
      END FUNCTION domain_get_time_since_sim_start




      SUBROUTINE domain_clock_get( grid, current_time,                &
                                         current_timestr,             &
                                         current_timestr_frac,        &
                                         start_time, start_timestr,   &
                                         stop_time, stop_timestr,     &
                                         time_step, time_stepstr,     &
                                         time_stepstr_frac,           &
                                         advanceCount,                &
                                         currentDayOfYearReal,        &
                                         minutesSinceSimulationStart, &
                                         timeSinceSimulationStart,    &
                                         simulationStartTime,         &
                                         simulationStartTimeStr )
        IMPLICIT NONE
        TYPE(domain),            INTENT(IN)              :: grid
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: current_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr_frac
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: start_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: start_timestr
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: stop_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: stop_timestr
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: time_step
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr_frac
        INTEGER,                 INTENT(  OUT), OPTIONAL :: advanceCount
        
        
        REAL,                    INTENT(  OUT), OPTIONAL :: currentDayOfYearReal
        
        
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: simulationStartTime
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: simulationStartTimeStr
        
        
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: timeSinceSimulationStart
        
        REAL,                    INTENT(  OUT), OPTIONAL :: minutesSinceSimulationStart






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime, lcl_starttime
        TYPE(WRFU_Time) :: lcl_simulationStartTime
        TYPE(WRFU_TimeInterval) :: lcl_time_step, lcl_timeSinceSimulationStart
        INTEGER :: days, seconds, Sn, Sd, rc
        CHARACTER (LEN=256) :: tmp_str
        CHARACTER (LEN=256) :: frac_str
        REAL(WRFU_KIND_R8) :: currentDayOfYearR8
        IF ( PRESENT( start_time ) ) THEN
          start_time = domain_get_start_time ( grid )
        ENDIF
        IF ( PRESENT( start_timestr ) ) THEN
          lcl_starttime = domain_get_start_time ( grid )
          CALL wrf_timetoa ( lcl_starttime, start_timestr )
        ENDIF
        IF ( PRESENT( time_step ) ) THEN
          time_step = domain_get_time_step ( grid )
        ENDIF
        IF ( PRESENT( time_stepstr ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, &
                                     timeString=time_stepstr, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22684,&
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_stepstr_frac ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, timeString=tmp_str, &
                                     Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22693,&
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          time_stepstr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( advanceCount ) ) THEN
          advanceCount = domain_get_advanceCount ( grid )
        ENDIF
        
        
        
        
        
        
        IF ( PRESENT( current_time ) ) THEN
          current_time = domain_get_current_time ( grid )
        ENDIF
        IF ( PRESENT( current_timestr ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, current_timestr )
        ENDIF
        
        IF ( PRESENT( current_timestr_frac ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, tmp_str )
          CALL WRFU_TimeGet( lcl_currtime, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22721,&
              'domain_clock_get:  WRFU_TimeGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          current_timestr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( stop_time ) ) THEN
          stop_time = domain_get_stop_time ( grid )
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          lcl_stoptime = domain_get_stop_time ( grid )
          CALL wrf_timetoa ( lcl_stoptime, stop_timestr )
        ENDIF
        IF ( PRESENT( currentDayOfYearReal ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL WRFU_TimeGet( lcl_currtime, dayOfYear_r8=currentDayOfYearR8, &
                             rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22739,&
                   'domain_clock_get:  WRFU_TimeGet(dayOfYear_r8) failed' )
          ENDIF
          currentDayOfYearReal = REAL( currentDayOfYearR8 ) - 1.0
        ENDIF
        IF ( PRESENT( simulationStartTime ) ) THEN
          simulationStartTime = domain_get_sim_start_time( grid )
        ENDIF
        IF ( PRESENT( simulationStartTimeStr ) ) THEN
          lcl_simulationStartTime = domain_get_sim_start_time( grid )
          CALL wrf_timetoa ( lcl_simulationStartTime, simulationStartTimeStr )
        ENDIF
        IF ( PRESENT( timeSinceSimulationStart ) ) THEN
          timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
        ENDIF
        IF ( PRESENT( minutesSinceSimulationStart ) ) THEN
          lcl_timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
          CALL WRFU_TimeIntervalGet( lcl_timeSinceSimulationStart, &
                                     D=days, S=seconds, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22759,&
                   'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          
          minutesSinceSimulationStart = ( REAL( days ) * 24. * 60. ) + &
                                        ( REAL( seconds ) / 60. )
          IF ( Sd /= 0 ) THEN
            minutesSinceSimulationStart = minutesSinceSimulationStart + &
                                          ( ( REAL( Sn ) / REAL( Sd ) ) / 60. )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_get

      FUNCTION domain_clockisstarttime ( grid ) RESULT ( is_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_start_time
        TYPE(WRFU_Time) :: start_time, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     start_time=start_time )
        is_start_time = ( current_time == start_time )
      END FUNCTION domain_clockisstarttime

      FUNCTION domain_clockissimstarttime ( grid ) RESULT ( is_sim_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_sim_start_time
        TYPE(WRFU_Time) :: simulationStartTime, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     simulationStartTime=simulationStartTime )
        is_sim_start_time = ( current_time == simulationStartTime )
      END FUNCTION domain_clockissimstarttime




      SUBROUTINE domain_clock_create( grid, StartTime, &
                                            StopTime,  &
                                            TimeStep )
        IMPLICIT NONE
        TYPE(domain),            INTENT(INOUT) :: grid
        TYPE(WRFU_Time),         INTENT(IN   ) :: StartTime
        TYPE(WRFU_Time),         INTENT(IN   ) :: StopTime
        TYPE(WRFU_TimeInterval), INTENT(IN   ) :: TimeStep





        
        INTEGER :: rc
        grid%domain_clock = WRFU_ClockCreate( TimeStep= TimeStep,  &
                                              StartTime=StartTime, &
                                              StopTime= StopTime,  &
                                              rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22828,&
            'domain_clock_create:  WRFU_ClockCreate() failed' )
        ENDIF
        grid%domain_clock_created = .TRUE.
        RETURN
      END SUBROUTINE domain_clock_create



      SUBROUTINE domain_alarm_create( grid, alarm_id, interval, &
                                            begin_time, end_time )
        USE module_utility
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid
        INTEGER, INTENT(IN) :: alarm_id
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: interval
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: begin_time
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: end_time





        
        INTEGER :: rc




        LOGICAL :: interval_only, all_args, no_args
        TYPE(WRFU_Time) :: startTime
        interval_only = .FALSE.
        all_args = .FALSE.
        no_args = .FALSE.
        IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
             ( .NOT. PRESENT( end_time   ) ) .AND. &
             (       PRESENT( interval   ) ) ) THEN
           interval_only = .TRUE.
        ELSE IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
                  ( .NOT. PRESENT( end_time   ) ) .AND. &
                  ( .NOT. PRESENT( interval   ) ) ) THEN
           no_args = .TRUE.
        ELSE IF ( (       PRESENT( begin_time ) ) .AND. &
                  (       PRESENT( end_time   ) ) .AND. &
                  (       PRESENT( interval   ) ) ) THEN
           all_args = .TRUE.
        ELSE
           CALL wrf_error_fatal3("<stdin>",22875,&
             'ERROR in domain_alarm_create:  bad argument list' )
        ENDIF
        CALL domain_clock_get( grid, start_time=startTime )
        IF ( interval_only ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingInterval=interval,   &
                               rc=rc )
        ELSE IF ( no_args ) THEN
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingTime=startTime,      &
                               rc=rc )
        ELSE IF ( all_args ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock,         &
                               RingTime=startTime + begin_time, &
                               RingInterval=interval,           &
                               StopTime=startTime + end_time,   &
                               rc=rc )
        ENDIF
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22900,&
            'domain_alarm_create:  WRFU_AlarmCreate() failed' )
        ENDIF
        CALL WRFU_AlarmRingerOff( grid%alarms( alarm_id ) , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22905,&
            'domain_alarm_create:  WRFU_AlarmRingerOff() failed' )
        ENDIF
        grid%alarms_created( alarm_id ) = .TRUE.
      END SUBROUTINE domain_alarm_create



      SUBROUTINE domain_clock_set( grid, current_timestr, &
                                         stop_timestr,    &
                                         time_step_seconds )
        IMPLICIT NONE
        TYPE(domain),      INTENT(INOUT)           :: grid
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: current_timestr
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: stop_timestr
        INTEGER,           INTENT(IN   ), OPTIONAL :: time_step_seconds






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime
        TYPE(WRFU_TimeInterval) :: tmpTimeInterval
        INTEGER :: rc
        IF ( PRESENT( current_timestr ) ) THEN
          CALL wrf_atotime( current_timestr(1:19), lcl_currtime )
          CALL WRFU_ClockSet( grid%domain_clock, currTime=lcl_currtime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22936,&
              'domain_clock_set:  WRFU_ClockSet(CurrTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          CALL wrf_atotime( stop_timestr(1:19), lcl_stoptime )
          CALL WRFU_ClockSet( grid%domain_clock, stopTime=lcl_stoptime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22945,&
              'domain_clock_set:  WRFU_ClockSet(StopTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_step_seconds ) ) THEN
          CALL WRFU_TimeIntervalSet( tmpTimeInterval, &
                                     S=time_step_seconds, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22953,&
              'domain_clock_set:  WRFU_TimeIntervalSet failed' )
          ENDIF
          CALL WRFU_ClockSet ( grid%domain_clock,        &
                               timeStep=tmpTimeInterval, &
                               rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",22960,&
              'domain_clock_set:  WRFU_ClockSet(TimeStep) failed' )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_set


      
      
      SUBROUTINE domain_clockprint ( level, grid, pre_str )
        IMPLICIT NONE
        INTEGER,           INTENT( IN) :: level
        TYPE(domain),      INTENT( IN) :: grid
        CHARACTER (LEN=*), INTENT( IN) :: pre_str
        CALL wrf_clockprint ( level, grid%domain_clock, pre_str )
        RETURN
      END SUBROUTINE domain_clockprint


      
      
      SUBROUTINE domain_clockadvance ( grid )
        IMPLICIT NONE
        TYPE(domain), INTENT(INOUT) :: grid
        INTEGER :: rc
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  before WRFU_ClockAdvance,' )
        CALL WRFU_ClockAdvance( grid%domain_clock, rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",22990,&
            'domain_clockadvance:  WRFU_ClockAdvance() failed' )
        ENDIF
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  after WRFU_ClockAdvance,' )
        
        
        CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
        CALL domain_clock_get( grid, currentDayOfYearReal=grid%julian )
        RETURN
      END SUBROUTINE domain_clockadvance



      
      
      SUBROUTINE domain_setgmtetc ( grid, start_of_simulation )
        IMPLICIT NONE
        TYPE (domain), INTENT(INOUT) :: grid
        LOGICAL,       INTENT(  OUT) :: start_of_simulation
        
        CHARACTER (LEN=132)          :: message
        TYPE(WRFU_Time)              :: simStartTime
        INTEGER                      :: hr, mn, sec, ms, rc
        CALL domain_clockprint(150, grid, &
          'DEBUG domain_setgmtetc():  get simStartTime from clock,')
        CALL domain_clock_get( grid, simulationStartTime=simStartTime, &
                                     simulationStartTimeStr=message )
        CALL WRFU_TimeGet( simStartTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",23021,&
            'domain_setgmtetc:  WRFU_TimeGet() failed' )
        ENDIF
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  simulation start time = [',TRIM( message ),']'
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  julyr,hr,mn,sec,ms,julday = ', &
                                     grid%julyr,hr,mn,sec,ms,grid%julday
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        start_of_simulation = domain_ClockIsSimStartTime(grid)
        RETURN
      END SUBROUTINE domain_setgmtetc
     


      
      
      SUBROUTINE set_current_grid_ptr( grid_ptr )
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid_ptr






        current_grid_set = .TRUE.
        current_grid => grid_ptr

      END SUBROUTINE set_current_grid_ptr








      LOGICAL FUNCTION Is_alarm_tstep( grid_clock, alarm )

        IMPLICIT NONE

        TYPE (WRFU_Clock), INTENT(in)  :: grid_clock
        TYPE (WRFU_Alarm), INTENT(in)  :: alarm

        LOGICAL :: pred1, pred2, pred3

        Is_alarm_tstep = .FALSE.

        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              IF ( alarm%alarmint%StopTimeSet ) THEN
                 PRED1 = ( grid_clock%clockint%CurrTime + grid_clock%clockint%TimeStep > &
                      alarm%alarmint%StopTime )
              ENDIF
              IF ( alarm%alarmint%RingTimeSet ) THEN
                 PRED2 = ( ( alarm%alarmint%RingTime - &
                      grid_clock%clockint%TimeStep <= &
                      grid_clock%clockint%CurrTime )     &
                      .AND. ( grid_clock%clockint%CurrTime < alarm%alarmint%RingTime ) )
              ENDIF
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                 PRED3 = ( alarm%alarmint%PrevRingTime + &
                      alarm%alarmint%RingInterval <= &
                      grid_clock%clockint%CurrTime + grid_clock%clockint%TimeStep )
              ENDIF
              IF ( ( .NOT. ( pred1 ) ) .AND. &
                   ( ( pred2 ) .OR. ( pred3 ) ) ) THEN
                 Is_alarm_tstep = .TRUE.
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
              IF ( alarm%alarmint%RingTime -&
                   grid_clock%clockint%TimeStep <= &
                   grid_clock%clockint%CurrTime ) THEN
                 Is_alarm_tstep = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

      END FUNCTION Is_alarm_tstep








      
      SUBROUTINE domain_time_test_print ( pre_str, name_str, res_str )
        IMPLICIT NONE
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        CHARACTER (LEN=*), INTENT(IN) :: name_str
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=512) :: out_str
        WRITE (out_str,                                            &
          FMT="('DOMAIN_TIME_TEST ',A,':  ',A,' = ',A)") &
          TRIM(pre_str), TRIM(name_str), TRIM(res_str)
        CALL wrf_debug( 0, TRIM(out_str) )
      END SUBROUTINE domain_time_test_print

      
      SUBROUTINE test_adjust_io_timestr( TI_h, TI_m, TI_s, &
        CT_yy,  CT_mm,  CT_dd,  CT_h,  CT_m,  CT_s,        &
        ST_yy,  ST_mm,  ST_dd,  ST_h,  ST_m,  ST_s,        &
        res_str, testname )
        INTEGER, INTENT(IN) :: TI_H
        INTEGER, INTENT(IN) :: TI_M
        INTEGER, INTENT(IN) :: TI_S
        INTEGER, INTENT(IN) :: CT_YY
        INTEGER, INTENT(IN) :: CT_MM  
        INTEGER, INTENT(IN) :: CT_DD  
        INTEGER, INTENT(IN) :: CT_H
        INTEGER, INTENT(IN) :: CT_M
        INTEGER, INTENT(IN) :: CT_S
        INTEGER, INTENT(IN) :: ST_YY
        INTEGER, INTENT(IN) :: ST_MM  
        INTEGER, INTENT(IN) :: ST_DD  
        INTEGER, INTENT(IN) :: ST_H
        INTEGER, INTENT(IN) :: ST_M
        INTEGER, INTENT(IN) :: ST_S
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=*), INTENT(IN) :: testname
        
        TYPE(WRFU_TimeInterval) :: TI
        TYPE(WRFU_Time) :: CT, ST
        LOGICAL :: test_passed
        INTEGER :: rc
        CHARACTER(LEN=WRFU_MAXSTR) :: TI_str, CT_str, ST_str, computed_str
        
        CALL WRFU_TimeIntervalSet( TI, H=TI_H, M=TI_M, S=TI_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeIntervalSet() ', &
                              "module_domain.F" , &
                              2675  )
        CALL WRFU_TimeIntervalGet( TI, timeString=TI_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2680  )
        
        CALL WRFU_TimeSet( CT, YY=CT_YY, MM=CT_MM, DD=CT_DD , &
                                H=CT_H,   M=CT_M,   S=CT_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2687  )
        CALL WRFU_TimeGet( CT, timeString=CT_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2692  )
        
        CALL WRFU_TimeSet( ST, YY=ST_YY, MM=ST_MM, DD=ST_DD , &
                                H=ST_H,   M=ST_M,   S=ST_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2699  )
        CALL WRFU_TimeGet( ST, timeString=ST_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2704  )
        
        CALL adjust_io_timestr ( TI, CT, ST, computed_str )
        
        test_passed = .FALSE.
        IF ( LEN_TRIM(res_str) == LEN_TRIM(computed_str) ) THEN
          IF ( res_str(1:LEN_TRIM(res_str)) == computed_str(1:LEN_TRIM(computed_str)) ) THEN
            test_passed = .TRUE.
          ENDIF
        ENDIF
        
        IF ( test_passed ) THEN
          WRITE(*,FMT='(A)') 'PASS:  '//TRIM(testname)
        ELSE
          WRITE(*,*) 'FAIL:  ',TRIM(testname),':  adjust_io_timestr(',    &
            TRIM(TI_str),',',TRIM(CT_str),',',TRIM(ST_str),')  expected <', &
            TRIM(res_str),'>  but computed <',TRIM(computed_str),'>'
        ENDIF
      END SUBROUTINE test_adjust_io_timestr

      
      
      
      
      
      SUBROUTINE domain_time_test ( grid, pre_str )
        IMPLICIT NONE
        TYPE(domain),      INTENT(IN) :: grid
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        
        LOGICAL, SAVE :: one_time_tests_done = .FALSE.
        REAL :: minutesSinceSimulationStart
        INTEGER :: advance_count, rc
        REAL :: currentDayOfYearReal
        TYPE(WRFU_TimeInterval) :: timeSinceSimulationStart
        TYPE(WRFU_Time) :: simulationStartTime
        CHARACTER (LEN=512) :: res_str
        LOGICAL :: self_test_domain
        
        
        
        
        
        
        CALL nl_get_self_test_domain( 1, self_test_domain )
        IF ( self_test_domain ) THEN
          CALL domain_clock_get( grid, advanceCount=advance_count )
          WRITE ( res_str, FMT="(I8.8)" ) advance_count
          CALL domain_time_test_print( pre_str, 'advanceCount', res_str )
          CALL domain_clock_get( grid, currentDayOfYearReal=currentDayOfYearReal )
          WRITE ( res_str, FMT='(F10.6)' ) currentDayOfYearReal
          CALL domain_time_test_print( pre_str, 'currentDayOfYearReal', res_str )
          CALL domain_clock_get( grid, minutesSinceSimulationStart=minutesSinceSimulationStart )
          WRITE ( res_str, FMT='(F10.6)' ) minutesSinceSimulationStart
          CALL domain_time_test_print( pre_str, 'minutesSinceSimulationStart', res_str )
          CALL domain_clock_get( grid, current_timestr=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr', res_str )
          CALL domain_clock_get( grid, current_timestr_frac=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr_frac', res_str )
          CALL domain_clock_get( grid, timeSinceSimulationStart=timeSinceSimulationStart )
          CALL WRFU_TimeIntervalGet( timeSinceSimulationStart, timeString=res_str, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",23251,&
              'domain_time_test:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL domain_time_test_print( pre_str, 'timeSinceSimulationStart', res_str )
          
          
          IF ( .NOT. one_time_tests_done ) THEN
            one_time_tests_done = .TRUE.
            CALL domain_clock_get( grid, simulationStartTimeStr=res_str )
            CALL domain_time_test_print( pre_str, 'simulationStartTime', res_str )
            CALL domain_clock_get( grid, start_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'start_timestr', res_str )
            CALL domain_clock_get( grid, stop_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'stop_timestr', res_str )
            CALL domain_clock_get( grid, time_stepstr=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr', res_str )
            CALL domain_clock_get( grid, time_stepstr_frac=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr_frac', res_str )
            
            
            
            
            
            
            CALL test_adjust_io_timestr( TI_h=3, TI_m=0, TI_s=0,          &
              CT_yy=2000,  CT_mm=1,  CT_dd=26,  CT_h=0,  CT_m=0,  CT_s=0, &
              ST_yy=2000,  ST_mm=1,  ST_dd=24,  ST_h=12, ST_m=0,  ST_s=0, &
              res_str='2000-01-26_00:00:00', testname='adjust_io_timestr_1' )
            
            
            
            
            
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_time_test






END MODULE module_domain









SUBROUTINE get_current_time_string( time_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: time_str
  
  INTEGER :: debug_level_lcl

  time_str = ''
  IF ( current_grid_set ) THEN








    IF ( current_grid%time_set ) THEN

      
      CALL get_wrf_debug_level( debug_level_lcl )
      CALL set_wrf_debug_level ( 0 )
      current_grid_set = .FALSE.
      CALL domain_clock_get( current_grid, current_timestr_frac=time_str )
      
      CALL set_wrf_debug_level ( debug_level_lcl )
      current_grid_set = .TRUE.

    ENDIF
  ENDIF

END SUBROUTINE get_current_time_string






SUBROUTINE get_current_grid_name( grid_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
  IF ( current_grid_set ) THEN
    WRITE(grid_str,FMT="('d',I2.2)") current_grid%id
  ENDIF
END SUBROUTINE get_current_grid_name




   SUBROUTINE get_ijk_from_grid_ext (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_domain
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid_ext




   SUBROUTINE get_ijk_from_subgrid_ext (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    USE module_domain
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid_ext


   SUBROUTINE get_dims_from_grid_id (  id   &
                          ,ds, de           &
                          ,ms, me           &
                          ,ps, pe           &
                          ,mxs, mxe         &
                          ,pxs, pxe         &
                          ,mys, mye         &
                          ,pys, pye )
    USE module_domain, ONLY : domain, head_grid, find_grid_by_id
    IMPLICIT NONE
    TYPE( domain ), POINTER  :: grid
    INTEGER, INTENT(IN ) :: id
    INTEGER, DIMENSION(3), INTENT(INOUT) ::                   &
                           ds, de           &
                          ,ms, me           &
                          ,ps, pe           &
                          ,mxs, mxe         &
                          ,pxs, pxe         &
                          ,mys, mye         &
                          ,pys, pye

     
     CHARACTER*256 mess

     NULLIFY( grid )
     CALL find_grid_by_id ( id, head_grid, grid )

     IF ( ASSOCIATED(grid) ) THEN
           ds(1) = grid%sd31 ; de(1) = grid%ed31 ; ds(2) = grid%sd32 ; de(2) = grid%ed32 ; ds(3) = grid%sd33 ; de(3) = grid%ed33 ;
           ms(1) = grid%sm31 ; me(1) = grid%em31 ; ms(2) = grid%sm32 ; me(2) = grid%em32 ; ms(3) = grid%sm33 ; me(3) = grid%em33 ;
           ps(1) = grid%sp31 ; pe(1) = grid%ep31 ; ps(2) = grid%sp32 ; pe(2) = grid%ep32 ; ps(3) = grid%sp33 ; pe(3) = grid%ep33 ;
           mxs(1) = grid%sm31x ; mxe(1) = grid%em31x 
           mxs(2) = grid%sm32x ; mxe(2) = grid%em32x 
           mxs(3) = grid%sm33x ; mxe(3) = grid%em33x 
           pxs(1) = grid%sp31x ; pxe(1) = grid%ep31x 
           pxs(2) = grid%sp32x ; pxe(2) = grid%ep32x 
           pxs(3) = grid%sp33x ; pxe(3) = grid%ep33x
           mys(1) = grid%sm31y ; mye(1) = grid%em31y 
           mys(2) = grid%sm32y ; mye(2) = grid%em32y 
           mys(3) = grid%sm33y ; mye(3) = grid%em33y 
           pys(1) = grid%sp31y ; pye(1) = grid%ep31y 
           pys(2) = grid%sp32y ; pye(2) = grid%ep32y 
           pys(3) = grid%sp33y ; pye(3) = grid%ep33y 
     ELSE
        WRITE(mess,*)'internal error: get_ijk_from_grid_id: no such grid id:',id
        CALL wrf_error_fatal3("<stdin>",23505,&
TRIM(mess))
     ENDIF

   END SUBROUTINE get_dims_from_grid_id


   SUBROUTINE get_ijk_from_grid_id (  id ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_domain, ONLY : domain, head_grid, find_grid_by_id, get_ijk_from_grid
    IMPLICIT NONE
    TYPE( domain ), POINTER  :: grid
    INTEGER, INTENT(IN ) :: id
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey
     
     CHARACTER*256 mess

     NULLIFY( grid )
     CALL find_grid_by_id ( id, head_grid, grid )

     IF ( ASSOCIATED(grid) ) THEN
     CALL get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
     ELSE
        WRITE(mess,*)'internal error: get_ijk_from_grid_id: no such grid id:',id
        CALL wrf_error_fatal3("<stdin>",23549,&
TRIM(mess))
     ENDIF

   END SUBROUTINE get_ijk_from_grid_id



   SUBROUTINE modify_io_masks ( id )
     USE module_domain, ONLY : domain, modify_io_masks1, head_grid, find_grid_by_id
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: id
     TYPE(domain), POINTER :: grid
     CALL find_grid_by_id( id, head_grid, grid )
     IF ( ASSOCIATED( grid ) ) CALL modify_io_masks1( grid, id ) 
     RETURN 
   END SUBROUTINE modify_io_masks

