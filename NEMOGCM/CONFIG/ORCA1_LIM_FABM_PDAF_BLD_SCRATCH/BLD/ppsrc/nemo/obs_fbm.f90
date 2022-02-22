












MODULE obs_fbm
   !!======================================================================
   !!                       ***  MODULE obs_fbm  ***
   !! Observation operators : I/O + tools for feedback files
   !!======================================================================
   !! History : 
   !!             !  08-11  (K. Mogensen) Initial version
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   init_obfbdata     :  Initialize sizes in obfbdata structure
   !!   alloc_obfbdata    :  Allocate data in an obfbdata structure
   !!   dealloc_obfbdata  :  Dellocate data in an obfbdata structure
   !!   copy_obfbdata     :  Copy an obfbdata structure
   !!   subsamp_obfbdata  :  Sumsample an obfbdata structure
   !!   merge_obfbdata    :  Merge multiple obfbdata structures into an one.
   !!   write_obfbdata    :  Write an obfbdata structure into a netCDF file.
   !!   read_obfbdata     :  Read an obfbdata structure from a netCDF file.
   !!----------------------------------------------------------------------
   USE netcdf
   USE obs_utils      ! Various utilities for observation operators

   IMPLICIT NONE
   PUBLIC

   ! Type kinds for feedback data.

   INTEGER, PARAMETER :: fbsp = SELECTED_REAL_KIND( 6, 37) !: single precision
   INTEGER, PARAMETER :: fbdp = SELECTED_REAL_KIND(12,307) !: double precision

   ! Parameters for string lengths.

   INTEGER, PARAMETER    :: ilenwmo  = 8    !: Length of station identifier
   INTEGER, PARAMETER    :: ilentyp  = 4    !: Length of type
   INTEGER, PARAMETER    :: ilenname = 8    !: Length of variable names
   INTEGER, PARAMETER    :: ilengrid = 1    !: Grid (e.g. 'T') length
   INTEGER, PARAMETER    :: ilenjuld = 14   !: Lenght of reference julian date
   INTEGER, PARAMETER    :: idefnqcf = 2    !: Default number of words in QC
                                            !  flags
   INTEGER, PARAMETER    :: ilenlong = 128  !: Length of long name
   INTEGER, PARAMETER    :: ilenunit = 32   !: Length of units
   
   ! Missinge data indicators 
   
   INTEGER, PARAMETER    :: fbimdi = -99999   !: Integers
   REAL(fbsp), PARAMETER :: fbrmdi =  99999   !: Reals

   ! Output stream choice
   LOGICAL               :: ln_cl4 = .FALSE.  !: Logical switch for
                                              !: class 4 file outputs
 
   ! Main data structure for observation feedback data.

   TYPE obfbdata
      LOGICAL :: lalloc         !: Allocation status for data
      LOGICAL :: lgrid          !: Include grid search info
      INTEGER :: nvar           !: Number of variables
      INTEGER :: nobs           !: Number of observations
      INTEGER :: nlev           !: Number of levels
      INTEGER :: nadd           !: Number of additional entries
      INTEGER :: next           !: Number of extra variables
      INTEGER :: nqcf           !: Number of words per qc flag
      CHARACTER(LEN=ilenwmo), DIMENSION(:), POINTER :: &
         & cdwmo                !: Identifier
      CHARACTER(LEN=ilentyp), DIMENSION(:), POINTER :: &
         & cdtyp                !: Instrument type
      CHARACTER(LEN=ilenjuld) :: &
         & cdjuldref            !: Julian date reference
      INTEGER, DIMENSION(:), POINTER :: &
         & kindex               !: Index of observations in the original file
      INTEGER, DIMENSION(:), POINTER :: &
         & ioqc, &              !: Observation QC
         & ipqc, &              !: Position QC
         & itqc                 !: Time QC
      INTEGER, DIMENSION(:,:), POINTER :: &
         & ioqcf, &             !: Observation QC flags
         & ipqcf, &             !: Position QC flags
         & itqcf                !: Time QC flags
      INTEGER, DIMENSION(:,:), POINTER :: &
         & idqc                 !: Depth QC
      INTEGER, DIMENSION(:,:,:), POINTER :: &
         & idqcf                !: Depth QC flags
      REAL(KIND=fbdp), DIMENSION(:), POINTER :: &
         & plam, &              !: Longitude
         & pphi, &              !: Latitude
         & ptim                 !: Time
      REAL(KIND=fbsp), DIMENSION(:,:), POINTER :: &
         & pdep                 !: Depth
      CHARACTER(LEN=ilenname), DIMENSION(:), POINTER  :: &
         & cname                !: Name of variable
      REAL(fbsp), DIMENSION(:,:,:), POINTER :: &
         & pob                  !: Observation
      CHARACTER(LEN=ilenlong), DIMENSION(:), POINTER :: &
         & coblong              !: Observation long name (for output)
      CHARACTER(LEN=ilenunit), DIMENSION(:), POINTER :: &
         & cobunit              !: Observation units (for output)
      INTEGER, DIMENSION(:,:), POINTER :: &
         & ivqc                 !: Variable QC
      INTEGER, DIMENSION(:,:,:), POINTER :: &
         & ivqcf                !: Variable QC flags
      INTEGER, DIMENSION(:,:,:), POINTER :: &
         & ivlqc                !: Variable level QC
      INTEGER, DIMENSION(:,:,:,:), POINTER :: &
         & ivlqcf               !: Variable level QC flags
      INTEGER, DIMENSION(:,:), POINTER :: &
         & iproc, &             !: Processor of obs (no I/O for this variable).
         & iobsi, &             !: Global i index
         & iobsj                !: Global j index
      INTEGER, DIMENSION(:,:,:), POINTER :: &
         & iobsk                !: k index
      CHARACTER(LEN=ilengrid), DIMENSION(:), POINTER  :: &
         & cgrid                !: Grid for this variable
      CHARACTER(LEN=ilenname), DIMENSION(:), POINTER :: &
         & caddname             !: Additional entries names
      CHARACTER(LEN=ilenlong), DIMENSION(:,:), POINTER :: &
         & caddlong             !: Additional entries long name (for output)
      CHARACTER(LEN=ilenunit), DIMENSION(:,:), POINTER :: &
         & caddunit             !: Additional entries units (for output)
      REAL(fbsp), DIMENSION(:,:,:,:)   , POINTER :: &
         & padd                 !: Additional entries
      CHARACTER(LEN=ilenname), DIMENSION(:), POINTER :: &
         & cextname             !: Extra variables names
      CHARACTER(LEN=ilenlong), DIMENSION(:), POINTER :: &
         & cextlong             !: Extra variables long name (for output)
      CHARACTER(LEN=ilenunit), DIMENSION(:), POINTER :: &
         & cextunit             !: Extra variables units (for output)
      REAL(fbsp), DIMENSION(:,:,:)   , POINTER :: &
         & pext                 !: Extra variables
   END TYPE obfbdata

   PRIVATE putvaratt_obfbdata

   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.3 , NEMO Consortium (2010)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE init_obfbdata( fbdata )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE init_obfbdata  ***
      !!
      !! ** Purpose :   Initialize sizes in obfbdata structure
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obfbdata) :: fbdata      ! obsfbdata structure

      fbdata%nvar   = 0
      fbdata%nobs   = 0
      fbdata%nlev   = 0
      fbdata%nadd   = 0
      fbdata%next   = 0
      fbdata%nqcf   = idefnqcf
      fbdata%lalloc = .FALSE.
      fbdata%lgrid  = .FALSE.

   END SUBROUTINE init_obfbdata
   
   SUBROUTINE alloc_obfbdata( fbdata, kvar, kobs, klev, kadd, kext, lgrid, &
      &                       kqcf)
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE alloc_obfbdata  ***
      !!
      !! ** Purpose :   Allocate data in an obfbdata structure
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obfbdata) ::  fbdata          ! obsfbdata structure to be allocated
      INTEGER, INTENT(IN) :: kvar        ! Number of variables
      INTEGER, INTENT(IN) :: kobs        ! Number of observations
      INTEGER, INTENT(IN) :: klev        ! Number of levels
      INTEGER, INTENT(IN) :: kadd        ! Number of additional entries
      INTEGER, INTENT(IN) :: kext        ! Number of extra variables
      LOGICAL, INTENT(IN) :: lgrid       ! Include grid search information
      INTEGER, OPTIONAL ::  kqcf         ! Number of words for QC flags
      !! * Local variables
      INTEGER :: ji
      INTEGER :: jv

      ! Check allocation status and deallocate previous allocated structures

      IF ( fbdata%lalloc ) THEN
         CALL dealloc_obfbdata( fbdata )
      ENDIF

      ! Set dimensions

      fbdata%lalloc = .TRUE.
      fbdata%nvar   = kvar
      fbdata%nobs   = kobs
      fbdata%nlev   = MAX( klev, 1 )
      fbdata%nadd   = kadd
      fbdata%next   = kext
      IF ( PRESENT(kqcf) ) THEN
         fbdata%nqcf = kqcf
      ELSE
         fbdata%nqcf = idefnqcf
      ENDIF

      ! Set data not depending on number of observations

      fbdata%cdjuldref  = REPEAT( 'X', ilenjuld )

      ! Allocate and initialize standard data 

      ALLOCATE( &
         & fbdata%cname(fbdata%nvar),   &
         & fbdata%coblong(fbdata%nvar), &
         & fbdata%cobunit(fbdata%nvar)  &
         & )
      DO ji = 1, fbdata%nvar
         WRITE(fbdata%cname(ji),'(A,I2.2)')'V_',ji
         fbdata%coblong(ji) = REPEAT( ' ', ilenlong )
         fbdata%cobunit(ji) = REPEAT( ' ', ilenunit )
      END DO

      ! Optionally also store grid search information
      
      IF ( lgrid ) THEN
         ALLOCATE ( &
            & fbdata%cgrid(fbdata%nvar) &
            & )
         fbdata%cgrid(:)      = REPEAT( 'X', ilengrid )
         fbdata%lgrid         = .TRUE.
      ENDIF
         
      ! Allocate and initialize additional entries if present
         
      IF ( fbdata%nadd > 0 ) THEN
         ALLOCATE( &
            & fbdata%caddname(fbdata%nadd),              &
            & fbdata%caddlong(fbdata%nadd, fbdata%nvar), &
            & fbdata%caddunit(fbdata%nadd, fbdata%nvar)  &
            & )
         DO ji = 1, fbdata%nadd
            WRITE(fbdata%caddname(ji),'(A,I2.2)')'A',ji
         END DO
         DO jv = 1, fbdata%nvar
            DO ji = 1, fbdata%nadd
               fbdata%caddlong(ji,jv) = REPEAT( ' ', ilenlong )
               fbdata%caddunit(ji,jv) = REPEAT( ' ', ilenunit )
            END DO
         END DO
      ENDIF
         
      ! Allocate and initialize additional variables if present
         
      IF ( fbdata%next > 0 ) THEN
         ALLOCATE( &
            & fbdata%cextname(fbdata%next), &
            & fbdata%cextlong(fbdata%next), &
            & fbdata%cextunit(fbdata%next)  &
            & )
         DO ji = 1, fbdata%next
            WRITE(fbdata%cextname(ji),'(A,I2.2)')'E_',ji
            fbdata%cextlong(ji) = REPEAT( ' ', ilenlong )
            fbdata%cextunit(ji) = REPEAT( ' ', ilenunit )
         END DO
      ENDIF

      ! Data depending on number of observations is only allocated if nobs>0

      IF ( fbdata%nobs > 0 ) THEN

         ALLOCATE( &
            & fbdata%cdwmo(fbdata%nobs),                                      &
            & fbdata%cdtyp(fbdata%nobs),                                      &
            & fbdata%ioqc(fbdata%nobs),                                       &
            & fbdata%ioqcf(fbdata%nqcf,fbdata%nobs),                          &
            & fbdata%ipqc(fbdata%nobs),                                       &
            & fbdata%ipqcf(fbdata%nqcf,fbdata%nobs),                          &
            & fbdata%itqc(fbdata%nobs),                                       &
            & fbdata%itqcf(fbdata%nqcf,fbdata%nobs),                          &
            & fbdata%idqc(fbdata%nlev,fbdata%nobs),                           &
            & fbdata%idqcf(fbdata%nqcf,fbdata%nlev,fbdata%nobs),              &
            & fbdata%plam(fbdata%nobs),                                       &
            & fbdata%pphi(fbdata%nobs),                                       &
            & fbdata%pdep(fbdata%nlev,fbdata%nobs),                           &
            & fbdata%ptim(fbdata%nobs),                                       &
            & fbdata%kindex(fbdata%nobs),                                     &
            & fbdata%ivqc(fbdata%nobs,fbdata%nvar),                           &
            & fbdata%ivqcf(fbdata%nqcf,fbdata%nobs,fbdata%nvar),              &
            & fbdata%ivlqc(fbdata%nlev,fbdata%nobs,fbdata%nvar),              &
            & fbdata%ivlqcf(fbdata%nqcf,fbdata%nlev,fbdata%nobs,fbdata%nvar), &
            & fbdata%pob(fbdata%nlev,fbdata%nobs,fbdata%nvar)                 &
            & )
         fbdata%kindex(:)       = fbimdi
         fbdata%cdwmo(:)        = REPEAT( 'X', ilenwmo )
         fbdata%cdtyp(:)        = REPEAT( 'X', ilentyp )
         fbdata%ioqc(:)         = fbimdi
         fbdata%ioqcf(:,:)      = fbimdi
         fbdata%ipqc(:)         = fbimdi
         fbdata%ipqcf(:,:)      = fbimdi
         fbdata%itqc(:)         = fbimdi
         fbdata%itqcf(:,:)      = fbimdi
         fbdata%idqc(:,:)       = fbimdi
         fbdata%idqcf(:,:,:)    = fbimdi
         fbdata%plam(:)         = fbrmdi
         fbdata%pphi(:)         = fbrmdi
         fbdata%pdep(:,:)       = fbrmdi
         fbdata%ptim(:)         = fbrmdi
         fbdata%ivqc(:,:)       = fbimdi
         fbdata%ivqcf(:,:,:)    = fbimdi
         fbdata%ivlqc(:,:,:)    = fbimdi
         fbdata%ivlqcf(:,:,:,:) = fbimdi
         fbdata%pob(:,:,:)      = fbrmdi
         
         ! Optionally also store grid search information
         
         IF ( lgrid ) THEN
            ALLOCATE ( &
               & fbdata%iproc(fbdata%nobs,fbdata%nvar),            &
               & fbdata%iobsi(fbdata%nobs,fbdata%nvar),            &
               & fbdata%iobsj(fbdata%nobs,fbdata%nvar),            &
               & fbdata%iobsk(fbdata%nlev,fbdata%nobs,fbdata%nvar) &
               & )
            fbdata%iproc(:,:)    = fbimdi
            fbdata%iobsi(:,:)    = fbimdi
            fbdata%iobsj(:,:)    = fbimdi
            fbdata%iobsk(:,:,:)  = fbimdi
            fbdata%lgrid         = .TRUE.
         ENDIF
         
         ! Allocate and initialize additional entries if present
         
         IF ( fbdata%nadd > 0 ) THEN
            ALLOCATE( &
               & fbdata%padd(fbdata%nlev,fbdata%nobs,fbdata%nadd,fbdata%nvar) &
               & )
            fbdata%padd(:,:,:,:) = fbrmdi
         ENDIF
         
         ! Allocate and initialize additional variables if present
         
         IF ( fbdata%next > 0 ) THEN
            ALLOCATE( &
               & fbdata%pext(fbdata%nlev,fbdata%nobs,fbdata%next) &
               & )
            fbdata%pext(:,:,:) = fbrmdi
         ENDIF

      ENDIF

   END SUBROUTINE alloc_obfbdata

   SUBROUTINE dealloc_obfbdata( fbdata )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE dealloc_obfbdata  ***
      !!
      !! ** Purpose :   Deallocate data in an obfbdata strucure
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obfbdata) :: fbdata      ! obsfbdata structure

      ! Deallocate data 

      DEALLOCATE( &
         & fbdata%cname,  &
         & fbdata%coblong,&
         & fbdata%cobunit &
         & )

      ! Deallocate optional grid search information
      
      IF ( fbdata%lgrid ) THEN
         DEALLOCATE ( &
            & fbdata%cgrid  &
            & )
      ENDIF

      ! Deallocate additional entries

      IF ( fbdata%nadd > 0 ) THEN
         DEALLOCATE( &
            & fbdata%caddname, &
            & fbdata%caddlong, &
            & fbdata%caddunit  &
            & )
      ENDIF

      ! Deallocate extra variables

      IF ( fbdata%next > 0 ) THEN
         DEALLOCATE( &
            & fbdata%cextname, &
            & fbdata%cextlong, &
            & fbdata%cextunit  &
            & )
      ENDIF

      ! Deallocate arrays depending on number of obs (if nobs>0 only).

      IF ( fbdata%nobs > 0 ) THEN

         DEALLOCATE( &
            & fbdata%cdwmo,  &
            & fbdata%cdtyp,  &
            & fbdata%ioqc,   &
            & fbdata%ioqcf,  &
            & fbdata%ipqc,   &
            & fbdata%ipqcf,  &
            & fbdata%itqc,   &
            & fbdata%itqcf,  &
            & fbdata%idqc,   &
            & fbdata%idqcf,  &
            & fbdata%plam,   &
            & fbdata%pphi,   &
            & fbdata%pdep,   &
            & fbdata%ptim,   &
            & fbdata%kindex, &
            & fbdata%ivqc,   &
            & fbdata%ivqcf,  &
            & fbdata%ivlqc,  &
            & fbdata%ivlqcf, &
            & fbdata%pob     &
            & )


         ! Deallocate optional grid search information
      
         IF ( fbdata%lgrid ) THEN
            DEALLOCATE ( &
               & fbdata%iproc, &
               & fbdata%iobsi, &
               & fbdata%iobsj, &
               & fbdata%iobsk  & 
               & )
         ENDIF

         ! Deallocate additional entries

         IF ( fbdata%nadd > 0 ) THEN
            DEALLOCATE( &
               & fbdata%padd       &
               & )
         ENDIF

         ! Deallocate extra variables

         IF ( fbdata%next > 0 ) THEN
            DEALLOCATE( &
               & fbdata%pext       &
               & )
         ENDIF

      ENDIF

      ! Reset arrays sizes

      fbdata%lalloc = .FALSE.
      fbdata%lgrid  = .FALSE.
      fbdata%nvar   = 0
      fbdata%nobs   = 0
      fbdata%nlev   = 0
      fbdata%nadd   = 0
      fbdata%next   = 0
   
   END SUBROUTINE dealloc_obfbdata

   SUBROUTINE copy_obfbdata( fbdata1, fbdata2, kadd, kext, lgrid, kqcf )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE copy_obfbdata  ***
      !!
      !! ** Purpose :   Copy an obfbdata structure
      !!
      !! ** Method  :   Copy all data from fbdata1 to fbdata2
      !!                If fbdata2 is allocated it needs to be compliant
      !!                with fbdata1.
      !!                Additional entries can be added by setting nadd
      !!                Additional extra fields can be added by setting next
      !!                Grid information can be included with lgrid=.true.
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obfbdata) :: fbdata1               ! Input obsfbdata structure
      TYPE(obfbdata) :: fbdata2               ! Output obsfbdata structure
      INTEGER, INTENT(IN), OPTIONAL :: kadd   ! Number of additional entries
      INTEGER, INTENT(IN), OPTIONAL :: kext   ! Number of extra variables
      INTEGER, INTENT(IN), OPTIONAL :: kqcf   ! Number of words per qc flags
      LOGICAL, OPTIONAL :: lgrid              ! Grid info on output file

      !! * Local variables
      INTEGER :: nadd
      INTEGER :: next
      INTEGER :: nqcf
      LOGICAL :: llgrid
      INTEGER :: jv
      INTEGER :: je
      INTEGER :: ji
      INTEGER :: jk
      INTEGER :: jq

      ! Check allocation status of fbdata1

      IF ( .NOT. fbdata1%lalloc ) THEN
         CALL fatal_error( 'copy_obfbdata: input data not allocated', &
            &              515 )
      ENDIF
      
      ! If nadd,next not specified use the ones from fbdata1
      ! Otherwise check that they have large than the original ones
      
      IF ( PRESENT(kadd) ) THEN
         nadd = kadd
         IF ( nadd < fbdata1%nadd ) THEN
            CALL warning    ( 'copy_obfbdata: ' // &
               &              'nadd smaller than input nadd', 525 )
         ENDIF
      ELSE
         nadd = fbdata1%nadd
      ENDIF
      IF ( PRESENT(kext) ) THEN
         next = kext
         IF ( next < fbdata1%next ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'next smaller than input next', 534 )
         ENDIF
      ELSE
         next = fbdata1%next
      ENDIF
      IF ( PRESENT(lgrid) ) THEN
         llgrid = lgrid
         IF ( fbdata1%lgrid .AND. (.NOT. llgrid) ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'switching off grid info not possible', &
               &              544 )
         ENDIF
      ELSE
         llgrid = fbdata1%lgrid
      ENDIF
      IF ( PRESENT(kqcf) ) THEN
         nqcf = kqcf
         IF ( nqcf < fbdata1%nqcf ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'nqcf smaller than input nqcf', 553 )
         ENDIF
      ELSE
         nqcf = fbdata1%nqcf
      ENDIF

      ! Check allocation status of fbdata2 and 
      ! a) check that it conforms in size if already allocated
      ! b) allocate it if not already allocated
      
      IF ( fbdata2%lalloc ) THEN
         IF ( fbdata1%nvar > fbdata2%nvar ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'output kvar smaller than input kvar', 566 )
         ENDIF
         IF ( fbdata1%nobs > fbdata2%nobs ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'output kobs smaller than input kobs', 570 )
         ENDIF
         IF ( fbdata1%nlev > fbdata2%nlev ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'output klev smaller than input klev', 574 )
         ENDIF
         IF ( fbdata1%nadd > fbdata2%nadd ) THEN
            CALL warning    ( 'copy_obfbdata: ' // &
               &              'output nadd smaller than input nadd', 578 )
         ENDIF
         IF ( fbdata1%next > fbdata2%next ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'output next smaller than input next', 582 )
         ENDIF
         IF ( fbdata1%lgrid .NEQV. fbdata2%lgrid ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'lgrid inconsistent', 586 )
         ENDIF
         IF ( fbdata1%next > fbdata2%next ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'output next smaller than input next', 590 )
         ENDIF
         IF ( fbdata1%nqcf > fbdata2%nqcf ) THEN
            CALL fatal_error( 'copy_obfbdata: ' // &
               &              'output  smaller than input kext', 594 )
         ENDIF
      ELSE
         CALL alloc_obfbdata( fbdata2, fbdata1%nvar, fbdata1%nobs, &
            &                 fbdata1%nlev, nadd, next, llgrid, kqcf = nqcf )
      ENDIF

      ! Copy the header data

      fbdata2%cdjuldref = fbdata1%cdjuldref

      DO ji = 1, fbdata1%nobs
         fbdata2%cdwmo(ji)  = fbdata1%cdwmo(ji)
         fbdata2%cdtyp(ji)  = fbdata1%cdtyp(ji)
         fbdata2%ioqc(ji)   = fbdata1%ioqc(ji)
         fbdata2%ipqc(ji)   = fbdata1%ipqc(ji)
         fbdata2%itqc(ji)   = fbdata1%itqc(ji)
         fbdata2%plam(ji)   = fbdata1%plam(ji)
         fbdata2%pphi(ji)   = fbdata1%pphi(ji)
         fbdata2%ptim(ji)   = fbdata1%ptim(ji)
         fbdata2%kindex(ji) = fbdata1%kindex(ji)
         DO jq = 1, fbdata1%nqcf
            fbdata2%ioqcf(jq,ji)  = fbdata1%ioqcf(jq,ji)
            fbdata2%ipqcf(jq,ji)  = fbdata1%ipqcf(jq,ji)
            fbdata2%itqcf(jq,ji)  = fbdata1%itqcf(jq,ji)
         END DO
         DO jk = 1, fbdata1%nlev
            fbdata2%idqc(jk,ji)  = fbdata1%idqc(jk,ji)
            fbdata2%pdep(jk,ji)  = fbdata1%pdep(jk,ji)
            DO jq = 1, fbdata1%nqcf
               fbdata2%idqcf(jq,jk,ji) = fbdata1%idqcf(jq,jk,ji)
            END DO
         END DO
      END DO

      ! Copy the variable data

      DO jv = 1, fbdata1%nvar
         fbdata2%cname(jv) = fbdata1%cname(jv)
         fbdata2%coblong(jv) = fbdata1%coblong(jv)
         fbdata2%cobunit(jv) = fbdata1%cobunit(jv)
         DO ji = 1, fbdata1%nobs
            fbdata2%ivqc(ji,jv)  = fbdata1%ivqc(ji,jv)
            DO jq = 1, fbdata1%nqcf
               fbdata2%ivqcf(jq,ji,jv) = fbdata1%ivqcf(jq,ji,jv)
            END DO
            DO jk = 1, fbdata1%nlev
               fbdata2%ivlqc(jk,ji,jv)  = fbdata1%ivlqc(jk,ji,jv)
               fbdata2%pob(jk,ji,jv)    = fbdata1%pob(jk,ji,jv)
               DO jq = 1, fbdata1%nqcf
                  fbdata2%ivlqcf(jq,jk,ji,jv) = fbdata1%ivlqcf(jq,jk,ji,jv)
               END DO
            END DO
         END DO
      END DO

      ! Copy grid information
      
      IF ( fbdata1%lgrid ) THEN
         DO jv = 1, fbdata1%nvar
            fbdata2%cgrid(jv) = fbdata1%cgrid(jv)
            DO ji = 1, fbdata1%nobs
               fbdata2%iproc(ji,jv) = fbdata1%iproc(ji,jv)
               fbdata2%iobsi(ji,jv) = fbdata1%iobsi(ji,jv)
               fbdata2%iobsj(ji,jv) = fbdata1%iobsj(ji,jv)
               DO jk = 1, fbdata1%nlev
                  fbdata2%iobsk(jk,ji,jv)  = fbdata1%iobsk(jk,ji,jv)
               END DO
            END DO
         END DO
      ENDIF

      ! Copy additional information
      
      DO je = 1, MIN( fbdata1%nadd, fbdata2%nadd )
         fbdata2%caddname(je) = fbdata1%caddname(je)
      END DO
      DO jv = 1, fbdata1%nvar
         DO je = 1, MIN( fbdata1%nadd, fbdata2%nadd )
            fbdata2%caddlong(je,jv) = fbdata1%caddlong(je,jv)
            fbdata2%caddunit(je,jv) = fbdata1%caddunit(je,jv)
            DO ji = 1, fbdata1%nobs
               DO jk = 1, fbdata1%nlev
                  fbdata2%padd(jk,ji,je,jv) = fbdata1%padd(jk,ji,je,jv)
               END DO
            END DO
         END DO
      END DO
      
      ! Copy extra information

      DO je = 1, fbdata1%next
         fbdata2%cextname(je) = fbdata1%cextname(je)
         fbdata2%cextlong(je) = fbdata1%cextlong(je)
         fbdata2%cextunit(je) = fbdata1%cextunit(je)
      END DO
      DO je = 1, fbdata1%next
         DO ji = 1, fbdata1%nobs
            DO jk = 1, fbdata1%nlev
               fbdata2%pext(jk,ji,je) = fbdata1%pext(jk,ji,je)
            END DO
         END DO
      END DO

   END SUBROUTINE copy_obfbdata

   SUBROUTINE subsamp_obfbdata( fbdata1, fbdata2, llvalid )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE susbamp_obfbdata  ***
      !!
      !! ** Purpose :   Subsample an obfbdata structure based on the
      !!                logical mask.
      !!
      !! ** Method  :   Copy all data from fbdata1 to fbdata2 if
      !!                llvalid(obs)==true
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      TYPE(obfbdata) :: fbdata1           ! Input obsfbdata structure
      TYPE(obfbdata) :: fbdata2           ! Output obsfbdata structure
      LOGICAL, DIMENSION(fbdata1%nobs) :: llvalid     ! Grid info on output file
      !! * Local variables
      INTEGER :: nobs
      INTEGER :: jv
      INTEGER :: je
      INTEGER :: ji
      INTEGER :: jk
      INTEGER :: jq
      INTEGER :: ij

      ! Check allocation status of fbdata1

      IF ( .NOT. fbdata1%lalloc ) THEN
         CALL fatal_error( 'copy_obfbdata: input data not allocated', &
            &              730 )
      ENDIF
      
      ! Check allocation status of fbdata2 and abort if already allocated
      
      IF ( fbdata2%lalloc ) THEN
         CALL fatal_error( 'subsample_obfbdata: ' // &
            &              'fbdata2 already allocated', 737 )
      ENDIF
      
      ! Count number of subsampled observations

      nobs = COUNT(llvalid)
      
      ! Allocate new data structure

      CALL alloc_obfbdata( fbdata2, fbdata1%nvar, nobs, &
         &                 fbdata1%nlev, fbdata1%nadd, fbdata1%next, &
         &                 fbdata1%lgrid, kqcf = fbdata1%nqcf )

      ! Copy the header data

      fbdata2%cdjuldref = fbdata1%cdjuldref
      
      ij = 0
      DO ji = 1, fbdata1%nobs
         IF ( llvalid(ji) ) THEN
            ij = ij +1
            fbdata2%cdwmo(ij)  = fbdata1%cdwmo(ji)
            fbdata2%cdtyp(ij)  = fbdata1%cdtyp(ji)
            fbdata2%ioqc(ij)   = fbdata1%ioqc(ji)
            fbdata2%ipqc(ij)   = fbdata1%ipqc(ji)
            fbdata2%itqc(ij)   = fbdata1%itqc(ji)
            fbdata2%plam(ij)   = fbdata1%plam(ji)
            fbdata2%pphi(ij)   = fbdata1%pphi(ji)
            fbdata2%ptim(ij)   = fbdata1%ptim(ji)
            fbdata2%kindex(ij) = fbdata1%kindex(ji)
            DO jq = 1, fbdata1%nqcf
               fbdata2%ioqcf(jq,ij)  = fbdata1%ioqcf(jq,ji)
               fbdata2%ipqcf(jq,ij)  = fbdata1%ipqcf(jq,ji)
               fbdata2%itqcf(jq,ij)  = fbdata1%itqcf(jq,ji)
            END DO
            DO jk = 1, fbdata1%nlev
               fbdata2%idqc(jk,ij)  = fbdata1%idqc(jk,ji)
               fbdata2%pdep(jk,ij)  = fbdata1%pdep(jk,ji)
               DO jq = 1, fbdata1%nqcf
                  fbdata2%idqcf(jq,jk,ij) = fbdata1%idqcf(jq,jk,ji)
               END DO
            END DO
         ENDIF
      END DO

      ! Copy the variable data

      DO jv = 1, fbdata1%nvar
         fbdata2%cname(jv) = fbdata1%cname(jv)
         fbdata2%coblong(jv) = fbdata1%coblong(jv)
         fbdata2%cobunit(jv) = fbdata1%cobunit(jv)
         ij = 0
         DO ji = 1, fbdata1%nobs
            IF ( llvalid(ji) ) THEN
               ij = ij + 1
               fbdata2%ivqc(ij,jv)  = fbdata1%ivqc(ji,jv)
               DO jq = 1, fbdata1%nqcf
                  fbdata2%ivqcf(jq,ij,jv) = fbdata1%ivqcf(jq,ji,jv)
               END DO
               DO jk = 1, fbdata1%nlev
                  fbdata2%ivlqc(jk,ij,jv)  = fbdata1%ivlqc(jk,ji,jv)
                  fbdata2%pob(jk,ij,jv)    = fbdata1%pob(jk,ji,jv)
                  DO jq = 1, fbdata1%nqcf
                     fbdata2%ivlqcf(jq,jk,ij,jv) = fbdata1%ivlqcf(jq,jk,ji,jv)
                  END DO
               END DO
            ENDIF
         END DO
      END DO

      ! Copy grid information
      
      IF ( fbdata1%lgrid ) THEN
         DO jv = 1, fbdata1%nvar
            fbdata2%cgrid(jv) = fbdata1%cgrid(jv)
            ij = 0
            DO ji = 1, fbdata1%nobs
               IF ( llvalid(ji) ) THEN
                  ij = ij + 1
                  fbdata2%iproc(ij,jv) = fbdata1%iproc(ji,jv)
                  fbdata2%iobsi(ij,jv) = fbdata1%iobsi(ji,jv)
                  fbdata2%iobsj(ij,jv) = fbdata1%iobsj(ji,jv)
                  DO jk = 1, fbdata1%nlev
                     fbdata2%iobsk(jk,ij,jv)  = fbdata1%iobsk(jk,ji,jv)
                  END DO
               ENDIF
            END DO
         END DO
      ENDIF

      ! Copy additional information
      
      DO je = 1, fbdata1%nadd
         fbdata2%caddname(je) = fbdata1%caddname(je)
      END DO
      DO jv = 1, fbdata1%nvar
         DO je = 1, fbdata1%nadd
            fbdata2%caddlong(je,jv) = fbdata1%caddlong(je,jv)
            fbdata2%caddunit(je,jv) = fbdata1%caddunit(je,jv)
            ij = 0
            DO ji = 1, fbdata1%nobs
               IF ( llvalid(ji) ) THEN
                  ij = ij + 1
                  DO jk = 1, fbdata1%nlev
                     fbdata2%padd(jk,ij,je,jv) = fbdata1%padd(jk,ji,je,jv)
                  END DO
               ENDIF
            END DO
         END DO
      END DO
      
      ! Copy extra information

      DO je = 1, fbdata1%next
         fbdata2%cextname(je) = fbdata1%cextname(je)
         fbdata2%cextlong(je) = fbdata1%cextlong(je)
         fbdata2%cextunit(je) = fbdata1%cextunit(je)
      END DO
      DO je = 1, fbdata1%next
         ij = 0
         DO ji = 1, fbdata1%nobs
            IF ( llvalid(ji) ) THEN
               ij = ij + 1
               DO jk = 1, fbdata1%nlev
                  fbdata2%pext(jk,ij,je) = fbdata1%pext(jk,ji,je)
               END DO
            ENDIF
         END DO
      END DO

   END SUBROUTINE subsamp_obfbdata

   SUBROUTINE merge_obfbdata( nsets, fbdatain, fbdataout, iset, inum, iind )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE merge_obfbdata  ***
      !!
      !! ** Purpose :   Merge multiple obfbdata structures into an one.
      !!
      !! ** Method  :   The order of elements is based on the indices in
      !!                iind.
      !!                All input data are assumed to be consistent. This
      !!                is assumed to be checked before calling this routine.
      !!                Likewise output data is assume to be consistent as 
      !!                well without error checking.
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER, INTENT(IN):: nsets      ! Number of input data sets 
      TYPE(obfbdata), DIMENSION(nsets) :: fbdatain  ! Input obsfbdata structure
      TYPE(obfbdata) :: fbdataout      ! Output obsfbdata structure
      INTEGER, INTENT(IN), DIMENSION(fbdataout%nobs) :: &
         & iset                 ! Set number for a given obs.
      INTEGER, INTENT(IN), DIMENSION(fbdataout%nobs) :: &
         & inum                 ! Number within set for an obs
      INTEGER, INTENT(IN), DIMENSION(fbdataout%nobs) :: &
         & iind                 ! Indices for copying.
      !! * Local variables

      INTEGER :: js
      INTEGER :: jo
      INTEGER :: jv
      INTEGER :: je
      INTEGER :: ji
      INTEGER :: jk
      INTEGER :: jq

      ! Check allocation status of fbdatain 
      
      DO js = 1, nsets
         IF ( .NOT. fbdatain(js)%lalloc ) THEN
            CALL fatal_error( 'merge_obfbdata: input data not allocated', &
               &              910 )
         ENDIF
      END DO

      ! Check allocation status of fbdataout
      
      IF ( .NOT.fbdataout%lalloc ) THEN
         CALL fatal_error( 'merge_obfbdata: output data not allocated', &
            &              918 )
      ENDIF

      ! Merge various names

      DO jv = 1, fbdatain(1)%nvar
         fbdataout%cname(jv) = fbdatain(1)%cname(jv)
         fbdataout%coblong(jv) = fbdatain(1)%coblong(jv)
         fbdataout%cobunit(jv) = fbdatain(1)%cobunit(jv)
         IF ( fbdatain(1)%lgrid ) THEN
            fbdataout%cgrid(jv) = fbdatain(1)%cgrid(jv)
         ENDIF
      END DO
      DO jv = 1, fbdatain(1)%nadd
         fbdataout%caddname(jv) = fbdatain(1)%caddname(jv)
      END DO
      DO jv = 1, fbdatain(1)%nvar
         DO je = 1, fbdatain(1)%nadd
            fbdataout%caddlong(je,jv) = fbdatain(1)%caddlong(je,jv)
            fbdataout%caddunit(je,jv) = fbdatain(1)%caddunit(je,jv)
         END DO
      END DO
      DO jv = 1, fbdatain(1)%next
         fbdataout%cextname(jv) = fbdatain(1)%cextname(jv)
         fbdataout%cextlong(jv) = fbdatain(1)%cextlong(jv)
         fbdataout%cextunit(jv) = fbdatain(1)%cextunit(jv)
      END DO
      fbdataout%cdjuldref = fbdatain(1)%cdjuldref

      ! Loop over total views

      DO jo = 1, fbdataout%nobs

         js = iset(iind(jo))
         ji = inum(iind(jo))

         ! Merge the header data

         fbdataout%cdwmo(jo)  = fbdatain(js)%cdwmo(ji)
         fbdataout%cdtyp(jo)  = fbdatain(js)%cdtyp(ji)
         fbdataout%ioqc(jo)   = fbdatain(js)%ioqc(ji)
         fbdataout%ipqc(jo)   = fbdatain(js)%ipqc(ji)
         fbdataout%itqc(jo)   = fbdatain(js)%itqc(ji)
         fbdataout%plam(jo)   = fbdatain(js)%plam(ji)
         fbdataout%pphi(jo)   = fbdatain(js)%pphi(ji)
         fbdataout%ptim(jo)   = fbdatain(js)%ptim(ji)
         fbdataout%kindex(jo) = fbdatain(js)%kindex(ji)
         DO jq = 1, fbdatain(js)%nqcf
            fbdataout%ioqcf(jq,jo)  = fbdatain(js)%ioqcf(jq,ji)
            fbdataout%ipqcf(jq,jo)  = fbdatain(js)%ipqcf(jq,ji)
            fbdataout%itqcf(jq,jo)  = fbdatain(js)%itqcf(jq,ji)
         END DO
         DO jk = 1, fbdatain(js)%nlev
            fbdataout%pdep(jk,jo)  = fbdatain(js)%pdep(jk,ji)
            fbdataout%idqc(jk,jo)  = fbdatain(js)%idqc(jk,ji)
            DO jq = 1, fbdatain(js)%nqcf
               fbdataout%idqcf(jq,jk,jo) = fbdatain(js)%idqcf(jq,jk,ji)
            END DO
         END DO

         ! Merge the variable data

         DO jv = 1, fbdatain(js)%nvar
            fbdataout%ivqc(jo,jv)  = fbdatain(js)%ivqc(ji,jv)
            DO jq = 1, fbdatain(js)%nqcf
               fbdataout%ivqcf(jq,jo,jv) = fbdatain(js)%ivqcf(jq,ji,jv)
            END DO
            DO jk = 1, fbdatain(js)%nlev
               fbdataout%ivlqc(jk,jo,jv)  = fbdatain(js)%ivlqc(jk,ji,jv)
               fbdataout%pob(jk,jo,jv)    = fbdatain(js)%pob(jk,ji,jv)
               DO jq = 1, fbdatain(js)%nqcf
                  fbdataout%ivlqcf(jq,jk,jo,jv) = &
                     &                     fbdatain(js)%ivlqcf(jq,jk,ji,jv)
               END DO
            END DO
         END DO

         ! Merge grid information
         
         IF ( fbdatain(js)%lgrid ) THEN
            DO jv = 1, fbdatain(js)%nvar
               fbdataout%cgrid(jv) = fbdatain(js)%cgrid(jv)
               fbdataout%iproc(jo,jv) = fbdatain(js)%iproc(ji,jv)
               fbdataout%iobsi(jo,jv) = fbdatain(js)%iobsi(ji,jv)
               fbdataout%iobsj(jo,jv) = fbdatain(js)%iobsj(ji,jv)
               DO jk = 1, fbdatain(js)%nlev
                  fbdataout%iobsk(jk,jo,jv)  = fbdatain(js)%iobsk(jk,ji,jv)
               END DO
            END DO
         ENDIF

         ! Merge additional information
      
         DO jv = 1, fbdatain(js)%nvar
            DO je = 1, fbdatain(js)%nadd
               DO jk = 1, fbdatain(js)%nlev
                  fbdataout%padd(jk,jo,je,jv) = fbdatain(js)%padd(jk,ji,je,jv)
               END DO
            END DO
         END DO
         
         ! Merge extra information
         
         DO je = 1, fbdatain(js)%next
            DO jk = 1, fbdatain(js)%nlev
               fbdataout%pext(jk,jo,je) = fbdatain(js)%pext(jk,ji,je)
            END DO
         END DO

      END DO

   END SUBROUTINE merge_obfbdata

   SUBROUTINE write_obfbdata( cdfilename, fbdata )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE write_obfbdata  ***
      !!
      !! ** Purpose :   Write an obfbdata structure into a netCDF file.
      !!
      !! ** Method  :   Decides which output wrapper to use. 
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=*) :: cdfilename ! Output filename
      TYPE(obfbdata)   :: fbdata     ! obsfbdata structure
          ! Standard feedback file output stream
          CALL write_obfbdata_fb( cdfilename, fbdata )
   END SUBROUTINE write_obfbdata

   SUBROUTINE write_obfbdata_fb( cdfilename, fbdata )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE write_obfbdata  ***
      !!
      !! ** Purpose :   Write an obfbdata structure into a netCDF file.
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=*) :: cdfilename ! Output filename
      TYPE(obfbdata)   :: fbdata     ! obsfbdata structure
      !! * Local variables
      CHARACTER(LEN=14), PARAMETER :: cpname = 'write_obfbdata'
      ! Dimension ids
      INTEGER :: idfile
      INTEGER :: idodim
      INTEGER :: idldim
      INTEGER :: idvdim
      INTEGER :: idadim
      INTEGER :: idedim
      INTEGER :: idsndim
      INTEGER :: idsgdim
      INTEGER :: idswdim
      INTEGER :: idstdim
      INTEGER :: idjddim
      INTEGER :: idqcdim
      INTEGER :: idvard
      INTEGER :: idaddd
      INTEGER :: idextd
      INTEGER :: idcdwmo
      INTEGER :: idcdtyp
      INTEGER :: idplam
      INTEGER :: idpphi
      INTEGER :: idpdep
      INTEGER :: idptim
      INTEGER :: idptimr
      INTEGER :: idioqc         
      INTEGER :: idioqcf         
      INTEGER :: idipqc
      INTEGER :: idipqcf
      INTEGER :: iditqc
      INTEGER :: iditqcf
      INTEGER :: ididqc
      INTEGER :: ididqcf
      INTEGER :: idkindex
      INTEGER, DIMENSION(fbdata%nvar) :: &
         & idpob,    &
         & idivqc,   &
         & idivqcf,  &
         & idivlqc,  &
         & idivlqcf, &
         & idiobsi,  &
         & idiobsj,  &
         & idiobsk,  &
         & idcgrid
      INTEGER, DIMENSION(fbdata%nadd,fbdata%nvar) :: idpadd
      INTEGER, DIMENSION(fbdata%next) :: idpext
      INTEGER, DIMENSION(1) :: incdim1
      INTEGER, DIMENSION(2) :: incdim2
      INTEGER, DIMENSION(3) :: incdim3
      INTEGER, DIMENSION(4) :: incdim4

      INTEGER :: jv
      INTEGER :: je
      INTEGER :: ioldfill
      CHARACTER(len=nf90_max_name) :: &
         & cdtmp
      CHARACTER(len=16), PARAMETER :: &
         & cdqcconv = 'q where q =[0,9]'
      CHARACTER(len=24), PARAMETER :: &
         & cdqcfconv = 'NEMOVAR flag conventions'
      CHARACTER(len=ilenlong) :: &
         & cdltmp

      ! Open output filename

      CALL chkerr( nf90_create( TRIM( cdfilename ), nf90_clobber, idfile ), &
         &         cpname, 1138 )
      CALL chkerr( nf90_set_fill( idfile, nf90_nofill, ioldfill ), &
         &         cpname, 1140 )
      CALL chkerr( nf90_put_att( idfile, nf90_global, 'title', &
         &                       'NEMO observation operator output' ), &
         &         cpname, 1143 )
      CALL chkerr( nf90_put_att( idfile, nf90_global, 'Convention', &
         &                       'NEMO unified observation operator output' ),&
         &         cpname,1146 )

      ! Create the dimensions

      CALL chkerr( nf90_def_dim( idfile, 'N_OBS'  , fbdata%nobs, idodim ),  &
         &         cpname,1151 )
      CALL chkerr( nf90_def_dim( idfile, 'N_LEVELS', fbdata%nlev, idldim ), &
         &         cpname,1153 )
      CALL chkerr( nf90_def_dim( idfile, 'N_VARS', fbdata%nvar, idvdim ), &
         &         cpname,1155 )
      CALL chkerr( nf90_def_dim( idfile, 'N_QCF', fbdata%nqcf, idqcdim ),&
         &         cpname,1157 )
      IF ( fbdata%nadd > 0 ) THEN
         CALL chkerr( nf90_def_dim( idfile, 'N_ENTRIES', fbdata%nadd, idadim ), &
            &         cpname,1160 )
      ENDIF
      IF ( fbdata%next > 0 ) THEN
         CALL chkerr( nf90_def_dim( idfile, 'N_EXTRA', fbdata%next, idedim ), &
            &         cpname,1164 )
      ENDIF
      CALL chkerr( nf90_def_dim( idfile, 'STRINGNAM', ilenname, idsndim ), &
         &         cpname,1167 )
      IF (fbdata%lgrid) THEN
         CALL chkerr( nf90_def_dim( idfile, 'STRINGGRID', ilengrid, idsgdim ),&
            &         cpname,1170 )
      ENDIF
      CALL chkerr( nf90_def_dim( idfile, 'STRINGWMO', ilenwmo, idswdim ), &
         &         cpname,1173 )
      CALL chkerr( nf90_def_dim( idfile, 'STRINGTYP', ilentyp, idstdim ), &
         &         cpname,1175 )
      CALL chkerr( nf90_def_dim( idfile, 'STRINGJULD', ilenjuld, idjddim ), &
         &         cpname,1177 )
      
      ! Define netCDF variables for header information
      
      incdim2(1) = idsndim
      incdim2(2) = idvdim

      CALL chkerr( nf90_def_var( idfile, 'VARIABLES', nf90_char, incdim2, &
         &                       idvard ), cpname, 1185 )
      CALL putvaratt_obfbdata( idfile, idvard, &
         &                     'List of variables in feedback files' )
      
      IF ( fbdata%nadd > 0 ) THEN
         incdim2(1) = idsndim
         incdim2(2) = idadim
         CALL chkerr( nf90_def_var( idfile, 'ENTRIES', nf90_char, incdim2, &
            &                       idaddd ), cpname, 1193 )
         CALL putvaratt_obfbdata( idfile, idaddd,  &
            &                     'List of additional entries for each '// &
            &                     'variable in feedback files' )
      ENDIF
   
      IF ( fbdata%next > 0 ) THEN
         incdim2(1) = idsndim
         incdim2(2) = idedim
         CALL chkerr( nf90_def_var( idfile, 'EXTRA', nf90_char, incdim2, &
            &                       idextd ), cpname, 1203 )
         CALL putvaratt_obfbdata(  idfile, idextd, &
            &                      'List of extra variables' )
      ENDIF

      incdim2(1) = idswdim
      incdim2(2) = idodim
      CALL chkerr( nf90_def_var( idfile, 'STATION_IDENTIFIER', &
         &                       nf90_char, incdim2, &
         &                       idcdwmo ), cpname, 1212 )
      CALL putvaratt_obfbdata(  idfile, idcdwmo, &
         &                      'Station identifier' )
      incdim2(1) = idstdim
      incdim2(2) = idodim
      CALL chkerr( nf90_def_var( idfile, 'STATION_TYPE', &
         &                       nf90_char, incdim2, &
         &                       idcdtyp ), cpname, 1219 )
      CALL putvaratt_obfbdata(  idfile, idcdtyp, &
         &                      'Code instrument type' )
      incdim1(1) = idodim
      CALL chkerr( nf90_def_var( idfile, 'LONGITUDE', &
         &                       nf90_double, incdim1, &
         &                       idplam ), cpname, 1225 )
      CALL putvaratt_obfbdata(  idfile, idplam, &
         &                      'Longitude', cdunits = 'degrees_east', &
         &                      rfillvalue = fbrmdi )
      CALL chkerr( nf90_def_var( idfile, 'LATITUDE', &
         &                       nf90_double, incdim1, &
         &                       idpphi ), cpname, 1231 )
      CALL putvaratt_obfbdata(  idfile, idpphi, &
         &                      'Latitude', cdunits = 'degrees_north', &
         &                      rfillvalue = fbrmdi )
      incdim2(1) = idldim
      incdim2(2) = idodim
      CALL chkerr( nf90_def_var( idfile, 'DEPTH', &
         &                       nf90_double, incdim2, &
         &                       idpdep ), cpname, 1239 )
      CALL putvaratt_obfbdata(  idfile, idpdep, &
         &                      'Depth', cdunits = 'metre', &
         &                      rfillvalue = fbrmdi )
      incdim3(1) = idqcdim
      incdim3(2) = idldim
      incdim3(3) = idodim
      CALL chkerr( nf90_def_var( idfile, 'DEPTH_QC', &
         &                       nf90_int, incdim2, &
         &                       ididqc ), cpname, 1248 )
      CALL putvaratt_obfbdata(  idfile, ididqc, &
         &                      'Quality on depth',  &
         &                      conventions = cdqcconv, &
         &                      ifillvalue = 0 )
      CALL chkerr( nf90_def_var( idfile, 'DEPTH_QC_FLAGS', &
         &                       nf90_int, incdim3, &
         &                       ididqcf ), cpname, 1255 )
      CALL putvaratt_obfbdata(  idfile, ididqcf, &
         &                      'Quality flags on depth',  &
         &                      conventions = cdqcfconv )
      CALL chkerr( nf90_def_var( idfile, 'JULD', &
         &                       nf90_double, incdim1, &
         &                       idptim ), cpname, 1261 )
      CALL putvaratt_obfbdata(  idfile, idptim, &
         &                      'Julian day', &
         &                      cdunits = 'days since JULD_REFERENCE', &
         &                      conventions = 'relative julian days with '// &
         &                                 'decimal part (as parts of day)', &
         &                      rfillvalue = fbrmdi )
      incdim1(1) = idjddim
      CALL chkerr( nf90_def_var( idfile, 'JULD_REFERENCE', &
         &                       nf90_char, incdim1, &
         &                       idptimr ), cpname, 1271 )
      CALL putvaratt_obfbdata(  idfile, idptimr, &
         &                      'Date of reference for julian days ', &
         &                      conventions = 'YYYYMMDDHHMMSS' )
      incdim1(1) = idodim
      CALL chkerr( nf90_def_var( idfile, 'OBSERVATION_QC', &
         &                       nf90_int, incdim1, &
         &                       idioqc ), cpname, 1278 )
      CALL putvaratt_obfbdata(  idfile, idioqc, &
         &                      'Quality on observation',  &
         &                      conventions = cdqcconv, &
         &                      ifillvalue = 0 )
      incdim2(1) = idqcdim
      incdim2(2) = idodim
      CALL chkerr( nf90_def_var( idfile, 'OBSERVATION_QC_FLAGS', &
         &                       nf90_int, incdim2, &
         &                       idioqcf ), cpname, 1287 )
      CALL putvaratt_obfbdata(  idfile, idioqcf, &
         &                      'Quality flags on observation',  &
         &                      conventions = cdqcfconv, &
         &                      ifillvalue = 0 )
      CALL chkerr( nf90_def_var( idfile, 'POSITION_QC', &
         &                       nf90_int, incdim1, &
         &                       idipqc ), cpname, 1294 )
      CALL putvaratt_obfbdata(  idfile, idipqc, &
         &                      'Quality on position (latitude and longitude)',  &
         &                      conventions = cdqcconv, &
         &                      ifillvalue = 0 )
      CALL chkerr( nf90_def_var( idfile, 'POSITION_QC_FLAGS', &
         &                       nf90_int, incdim2, &
         &                       idipqcf ), cpname, 1301 )
      CALL putvaratt_obfbdata(  idfile, idipqcf, &
         &                      'Quality flags on position',  &
         &                      conventions = cdqcfconv, &
         &                      ifillvalue = 0 )
      CALL chkerr( nf90_def_var( idfile, 'JULD_QC', &
         &                       nf90_int, incdim1, &
         &                       iditqc ), cpname, 1308 )
      CALL putvaratt_obfbdata(  idfile, iditqc, &
         &                      'Quality on date and time',  &
         &                      conventions = cdqcconv, &
         &                      ifillvalue = 0 )
      CALL chkerr( nf90_def_var( idfile, 'JULD_QC_FLAGS', &
         &                       nf90_int, incdim2, &
         &                       iditqcf ), cpname, 1315 )
      CALL putvaratt_obfbdata(  idfile, iditqcf, &
         &                      'Quality flags on date and time',  &
         &                      conventions = cdqcfconv, &
         &                      ifillvalue = 0 )
      CALL chkerr( nf90_def_var( idfile, 'ORIGINAL_FILE_INDEX', &
         &                       nf90_int, incdim1, &
         &                       idkindex ), cpname, 1322 )
      CALL putvaratt_obfbdata(  idfile, idkindex, &
         &                      'Index in original data file',  &
         &                      ifillvalue = fbimdi )

      ! Define netCDF variables for individual variables

      DO jv = 1, fbdata%nvar

         incdim1(1) = idodim
         incdim2(1) = idldim
         incdim2(2) = idodim
         WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_OBS'
         CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_float, &
            &                       incdim2, idpob(jv) ), &
            &         cpname, 1337 )
         CALL putvaratt_obfbdata(  idfile, idpob(jv), &
            &                      fbdata%coblong(jv),  &
            &                      cdunits =  fbdata%cobunit(jv), &
            &                      rfillvalue = fbrmdi )

         IF ( fbdata%nadd > 0 ) THEN
            DO je = 1, fbdata%nadd
               WRITE(cdtmp,'(3A)') TRIM(fbdata%cname(jv)),'_',&
                  &                TRIM(fbdata%caddname(je))
               CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_float, &
                  &                       incdim2, idpadd(je,jv) ), &
                  &         cpname, 1349 )
               CALL putvaratt_obfbdata(  idfile, idpadd(je,jv), &
                  &                      fbdata%caddlong(je,jv), &
                  &                      cdunits =  fbdata%caddunit(je,jv), &
                  &                      rfillvalue = fbrmdi )
            END DO
         ENDIF

         cdltmp = fbdata%coblong(jv)
         IF (( cdltmp(1:1) >= 'A' ).AND.( cdltmp(1:1) <= 'Z' )) &
            & cdltmp(1:1) = ACHAR(IACHAR(cdltmp(1:1)) + 32)
         WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_QC'
         CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
            &                       incdim1, idivqc(jv) ), &
            &         cpname, 1363 )
         CALL putvaratt_obfbdata(  idfile, idivqc(jv), &
            &                      'Quality on '//cdltmp,  &
            &                      conventions = cdqcconv, &
            &                      ifillvalue = 0 )
         incdim2(1) = idqcdim
         incdim2(2) = idodim
         WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_QC_FLAGS'
         CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
            &                       incdim2, idivqcf(jv) ), &
            &         cpname, 1373 )
         CALL putvaratt_obfbdata(  idfile, idivqcf(jv), &
            &                      'Quality flags on '//cdltmp,  &
            &                      conventions = cdqcfconv, &
            &                      ifillvalue = 0 )
         incdim2(1) = idldim
         incdim2(2) = idodim
         WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_LEVEL_QC'
         CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
            &                       incdim2, idivlqc(jv) ), &
            &         cpname, 1383 )
         CALL putvaratt_obfbdata(  idfile, idivlqc(jv), &
            &                      'Quality for each level on '//cdltmp,  &
            &                      conventions = cdqcconv, &
            &                      ifillvalue = 0 )
         incdim3(1) = idqcdim
         incdim3(2) = idldim
         incdim3(3) = idodim
         WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_LEVEL_QC_FLAGS'
         CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
            &                       incdim3, idivlqcf(jv) ), &
            &         cpname, 1394 )
         CALL putvaratt_obfbdata(  idfile, idivlqcf(jv), &
            &                      'Quality flags for each level on '//&
            &                      cdltmp,  &
            &                      conventions = cdqcfconv, &
            &                      ifillvalue = 0 )

         IF (fbdata%lgrid) THEN
            incdim2(1) = idldim
            incdim2(2) = idodim
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_IOBSI'
            CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
               &                       incdim1, idiobsi(jv) ), &
               &         cpname, 1407 )
            CALL putvaratt_obfbdata(  idfile, idiobsi(jv), &
               &                      'ORCA grid search I coordinate')
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_IOBSJ'
            CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
               &                       incdim1, idiobsj(jv) ), &
               &         cpname, 1413 )
            CALL putvaratt_obfbdata(  idfile, idiobsj(jv), &
               &                      'ORCA grid search J coordinate')
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_IOBSK'
            CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_int, &
               &                       incdim2, idiobsk(jv) ), &
               &         cpname, 1419 )
            CALL putvaratt_obfbdata(  idfile, idiobsk(jv), &
               &                      'ORCA grid search K coordinate')
            incdim1(1) = idsgdim
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_GRID'
            CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_char, incdim1, &
               &                       idcgrid(jv) ), cpname, 1425 )
            CALL putvaratt_obfbdata(  idfile, idcgrid(jv), &
               &                      'ORCA grid search grid (T,U,V)')
         ENDIF

      END DO

      IF ( fbdata%next > 0 ) THEN
         DO je = 1, fbdata%next
            incdim2(1) = idldim
            incdim2(2) = idodim
            WRITE(cdtmp,'(A)') TRIM(fbdata%cextname(je))
            CALL chkerr( nf90_def_var( idfile, cdtmp, nf90_float, &
               &                       incdim2, idpext(je) ), &
               &         cpname, 1439 )
            CALL putvaratt_obfbdata(  idfile, idpext(je), &
               &                      fbdata%cextlong(je),  &
               &                      cdunits =  fbdata%cextunit(je), &
               &                      rfillvalue = fbrmdi )
         END DO
      ENDIF
      
      ! Stop definitions

      CALL chkerr( nf90_enddef( idfile ), cpname, 1449 )
      
      ! Write the variables
      
      CALL chkerr( nf90_put_var( idfile, idvard, fbdata%cname ), &
         &         cpname, 1454 )
      
      IF ( fbdata%nadd > 0 ) THEN
         CALL chkerr( nf90_put_var( idfile, idaddd, fbdata%caddname ), &
            &         cpname, 1458 )
      ENDIF
      
      IF ( fbdata%next > 0 ) THEN
         CALL chkerr( nf90_put_var( idfile, idextd, fbdata%cextname ), &
            &         cpname, 1463 )
      ENDIF

      CALL chkerr( nf90_put_var( idfile, idptimr, fbdata%cdjuldref ), &
         &         cpname, 1467 )

      ! Only write the data if observation is available
      
      IF ( fbdata%nobs > 0 ) THEN

         CALL chkerr( nf90_put_var( idfile, idcdwmo, fbdata%cdwmo ), &
            &         cpname, 1474 )
         CALL chkerr( nf90_put_var( idfile, idcdtyp, fbdata%cdtyp ), &
            &         cpname, 1476 )
         CALL chkerr( nf90_put_var( idfile, idplam, fbdata%plam ), &
            &         cpname, 1478 )
         CALL chkerr( nf90_put_var( idfile, idpphi, fbdata%pphi ), &
            &         cpname, 1480 )
         CALL chkerr( nf90_put_var( idfile, idpdep, fbdata%pdep ), &
            &         cpname, 1482 )
         CALL chkerr( nf90_put_var( idfile, idptim, fbdata%ptim ), &
            &         cpname, 1484 )
         CALL chkerr( nf90_put_var( idfile, idioqc, fbdata%ioqc ), &
            &         cpname, 1486 )
         CALL chkerr( nf90_put_var( idfile, idioqcf, fbdata%ioqcf ), &
            &         cpname, 1488 )
         CALL chkerr( nf90_put_var( idfile, idipqc, fbdata%ipqc ), &
            &         cpname, 1490 )
         CALL chkerr( nf90_put_var( idfile, idipqcf, fbdata%ipqcf ), &
            &         cpname, 1492 )
         CALL chkerr( nf90_put_var( idfile, iditqc, fbdata%itqc ), &
            &         cpname, 1494 )
         CALL chkerr( nf90_put_var( idfile, iditqcf, fbdata%itqcf ), &
            &         cpname, 1496 )
         CALL chkerr( nf90_put_var( idfile, ididqc, fbdata%idqc ), &
            &         cpname, 1498 )
         CALL chkerr( nf90_put_var( idfile, ididqcf, fbdata%idqcf ), &
            &         cpname, 1500 )
         CALL chkerr( nf90_put_var( idfile, idkindex, fbdata%kindex ), &
            &         cpname, 1502 )

         DO jv = 1, fbdata%nvar
            CALL chkerr( nf90_put_var( idfile, idpob(jv), fbdata%pob(:,:,jv) ), &
               &         cpname, 1506 )
            IF ( fbdata%nadd > 0 ) THEN
               DO je = 1, fbdata%nadd
                  CALL chkerr( nf90_put_var( idfile, idpadd(je,jv), &
                     &                       fbdata%padd(:,:,je,jv) ), &
                     &         cpname, 1511 )
               END DO
            ENDIF
            CALL chkerr( nf90_put_var( idfile, idivqc(jv), &
               &                       fbdata%ivqc(:,jv) ),&
               &         cpname, 1516 )
            CALL chkerr( nf90_put_var( idfile, idivqcf(jv), &
               &                       fbdata%ivqcf(:,:,jv) ),&
               &         cpname, 1519 )
            CALL chkerr( nf90_put_var( idfile, idivlqc(jv), &
               &                       fbdata%ivlqc(:,:,jv) ),&
               &         cpname, 1522 )
            CALL chkerr( nf90_put_var( idfile, idivlqcf(jv), &
               &                       fbdata%ivlqcf(:,:,:,jv) ),&
               &         cpname, 1525 )
            IF (fbdata%lgrid) THEN
               CALL chkerr( nf90_put_var( idfile, idiobsi(jv), &
                  &                       fbdata%iobsi(:,jv) ),&
                  &         cpname, 1529 )
               CALL chkerr( nf90_put_var( idfile, idiobsj(jv), &
                  &                       fbdata%iobsj(:,jv) ),&
                  &         cpname, 1532 )
               CALL chkerr( nf90_put_var( idfile, idiobsk(jv), &
                  &                       fbdata%iobsk(:,:,jv) ),&
                  &         cpname, 1535 )
               CALL chkerr( nf90_put_var( idfile, idcgrid(jv), &
                  &                       fbdata%cgrid(jv) ), &
                  &         cpname, 1538 )
            ENDIF
         END DO

         IF ( fbdata%next > 0 ) THEN
            DO je = 1, fbdata%next
               CALL chkerr( nf90_put_var( idfile, idpext(je), &
                  &                       fbdata%pext(:,:,je) ), &
                  &         cpname, 1546 )
            END DO
         ENDIF

      ENDIF

      ! Close the file

      CALL chkerr( nf90_close( idfile ), cpname, 1554 )

      
   END SUBROUTINE write_obfbdata_fb



   SUBROUTINE putvaratt_obfbdata( idfile, idvar, cdlongname, cdunits, &
      &                           conventions, cfillvalue, &
      &                           ifillvalue, rfillvalue )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE putvaratt_obfbdata  ***
      !!
      !! ** Purpose :   Write netcdf attributes for variable
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER :: idfile                    ! File netcdf id.
      INTEGER :: idvar                     ! Variable netcdf id.
      CHARACTER(len=*) :: cdlongname       ! Long name for variable
      CHARACTER(len=*), OPTIONAL :: cdunits       ! Units for variable
      CHARACTER(len=*), OPTIONAL :: cfillvalue    ! Fill value for character variables
      INTEGER, OPTIONAL :: ifillvalue             ! Fill value for integer variables
      REAL(kind=fbsp), OPTIONAL :: rfillvalue     ! Fill value for real variables
      CHARACTER(len=*), OPTIONAL :: conventions   ! Conventions for variable
      !! * Local variables
      CHARACTER(LEN=18), PARAMETER :: &
         & cpname = 'putvaratt_obfbdata'

      CALL chkerr( nf90_put_att( idfile, idvar, 'long_name', &
         &                       TRIM(cdlongname) ), &
         &                       cpname, 1819 )
      
      IF ( PRESENT(cdunits) ) THEN

         CALL chkerr( nf90_put_att( idfile, idvar, 'units', &
            &                       TRIM(cdunits) ), &
            &                       cpname, 1825 )

      ENDIF

      IF ( PRESENT(conventions) ) THEN

         CALL chkerr( nf90_put_att( idfile, idvar, 'Conventions', &
            &                       TRIM(conventions) ), &
            &                       cpname, 1833 )

      ENDIF

      IF ( PRESENT(cfillvalue) ) THEN

         CALL chkerr( nf90_put_att( idfile, idvar, '_Fillvalue', &
            &                       TRIM(cfillvalue) ), &
            &                       cpname, 1841 )

      ENDIF

      IF ( PRESENT(ifillvalue) ) THEN

         CALL chkerr( nf90_put_att( idfile, idvar, '_Fillvalue', &
            &                       ifillvalue ), &
            &                       cpname, 1849 )

      ENDIF

      IF ( PRESENT(rfillvalue) ) THEN

         CALL chkerr( nf90_put_att( idfile, idvar, '_Fillvalue', &
            &                       rfillvalue ), &
            &                       cpname, 1857 )

      ENDIF

   END SUBROUTINE putvaratt_obfbdata

   SUBROUTINE read_obfbdata( cdfilename, fbdata, ldgrid )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE read_obfbdata  ***
      !!
      !! ** Purpose :   Read an obfbdata structure from a netCDF file.
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      CHARACTER(len=*) :: cdfilename  ! Input filename
      TYPE(obfbdata)   :: fbdata      ! obsfbdata structure
      LOGICAL, OPTIONAL :: ldgrid     ! Allow forcing of grid info
      !! * Local variables
      CHARACTER(LEN=14), PARAMETER :: cpname = 'read_obfbdata'
      INTEGER :: idfile
      INTEGER :: idodim
      INTEGER :: idldim
      INTEGER :: idvdim
      INTEGER :: idadim
      INTEGER :: idedim
      INTEGER :: idgdim
      INTEGER :: idvard
      INTEGER :: idaddd
      INTEGER :: idextd
      INTEGER :: idcdwmo
      INTEGER :: idcdtyp
      INTEGER :: idplam
      INTEGER :: idpphi
      INTEGER :: idpdep
      INTEGER :: idptim
      INTEGER :: idptimr
      INTEGER :: idioqc        
      INTEGER :: idioqcf
      INTEGER :: idipqc
      INTEGER :: idipqcf
      INTEGER :: ididqc
      INTEGER :: ididqcf
      INTEGER :: iditqc
      INTEGER :: iditqcf
      INTEGER :: idkindex
      INTEGER, DIMENSION(:), ALLOCATABLE :: &
         & idpob,    &
         & idivqc,   &
         & idivqcf,  &
         & idivlqc,  &
         & idivlqcf, &
         & idiobsi,  &
         & idiobsj,  &
         & idiobsk,  &
         & idcgrid,  &
         & idpext
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: &
         & idpadd
      INTEGER :: jv
      INTEGER :: je
      INTEGER :: nvar
      INTEGER :: nobs
      INTEGER :: nlev
      INTEGER :: nadd
      INTEGER :: next
      LOGICAL :: lgrid
      CHARACTER(len=NF90_MAX_NAME) :: cdtmp

      ! Check allocation status and deallocate previous allocated structures

      IF ( fbdata%lalloc ) THEN
         CALL dealloc_obfbdata( fbdata )
      ENDIF

      ! Open input filename

      CALL chkerr( nf90_open( TRIM( cdfilename ), nf90_nowrite, idfile ), &
         &         cpname, 1938 )

      ! Get input dimensions

      CALL chkerr( nf90_inq_dimid( idfile, 'N_OBS'  , idodim ),  &
         &         cpname,1943 )
      CALL chkerr( nf90_inquire_dimension( idfile, idodim, len=nobs ), &
         &         cpname,1945 )
      CALL chkerr( nf90_inq_dimid( idfile, 'N_LEVELS', idldim ), &
         &         cpname,1947 )
      CALL chkerr( nf90_inquire_dimension( idfile, idldim, len=nlev ), &
         &         cpname,1949 )
      CALL chkerr( nf90_inq_dimid( idfile, 'N_VARS', idvdim ), &
         &         cpname,1951 )
      CALL chkerr( nf90_inquire_dimension( idfile, idvdim, len=nvar ), &
         &         cpname,1953 )
      IF ( nf90_inq_dimid( idfile, 'N_ENTRIES',  idadim ) == 0 ) THEN
         CALL chkerr( nf90_inquire_dimension( idfile, idadim, len=nadd ), &
            &         cpname,1956 )
      ELSE
         nadd = 0
      ENDIF
      IF ( nf90_inq_dimid( idfile, 'N_EXTRA',  idedim ) == 0 ) THEN
         CALL chkerr( nf90_inquire_dimension( idfile, idedim, len=next ), &
            &         cpname,1962 )
      ELSE
         next = 0
      ENDIF
      !
      ! Check if this input file  contains grid search informations
      !
      lgrid = ( nf90_inq_dimid( idfile, 'STRINGGRID',  idgdim ) == 0 )

      ! Allocate data structure

      IF ( PRESENT(ldgrid) ) THEN
         CALL alloc_obfbdata( fbdata, nvar, nobs, nlev, nadd, next, &
            & lgrid.OR.ldgrid )
      ELSE
         CALL alloc_obfbdata( fbdata, nvar, nobs, nlev, nadd, next, &
            & lgrid )
      ENDIF

      ! Allocate netcdf identifiers

      ALLOCATE( &
         & idpob(fbdata%nvar),    &
         & idivqc(fbdata%nvar),   &
         & idivqcf(fbdata%nvar),  &
         & idivlqc(fbdata%nvar),  &
         & idivlqcf(fbdata%nvar), &
         & idiobsi(fbdata%nvar),  &
         & idiobsj(fbdata%nvar),  &
         & idiobsk(fbdata%nvar),  &
         & idcgrid(fbdata%nvar)   &
         & )
      IF ( fbdata%nadd > 0 ) THEN
         ALLOCATE( &
            & idpadd(fbdata%nadd,fbdata%nvar) &
            & )
      ENDIF
      IF ( fbdata%next > 0 ) THEN
         ALLOCATE( &
            & idpext(fbdata%next) &
            & )
      ENDIF

      ! Read variables for header information

      CALL chkerr( nf90_inq_varid( idfile, 'VARIABLES',idvard ), &
         &         cpname, 2008 )
      CALL chkerr( nf90_get_var( idfile, idvard, fbdata%cname ), &
         &         cpname, 2010 )
      IF ( fbdata%nadd > 0 ) THEN
         CALL chkerr( nf90_inq_varid( idfile, 'ENTRIES', idaddd ), &
            &         cpname, 2013 )
         CALL chkerr( nf90_get_var( idfile, idaddd, fbdata%caddname ), &
            &         cpname, 2015 )
      ENDIF
      IF ( fbdata%next > 0 ) THEN
         CALL chkerr( nf90_inq_varid( idfile, 'EXTRA', idextd ), &
            &         cpname, 2019 )
         CALL chkerr( nf90_get_var( idfile, idextd, fbdata%cextname ), &
            &         cpname, 2021 )
      ENDIF

      CALL chkerr( nf90_inq_varid( idfile, 'JULD_REFERENCE', idptimr ), &
         &         cpname, 2025 )
      CALL chkerr( nf90_get_var( idfile, idptimr, fbdata%cdjuldref ), &
         &         cpname, 2027 )

      IF  ( fbdata%nobs > 0 ) THEN
         
         CALL chkerr( nf90_inq_varid( idfile, 'STATION_IDENTIFIER', idcdwmo ),&
            &         cpname, 2032 )
         CALL chkerr( nf90_get_var( idfile, idcdwmo, fbdata%cdwmo ), &
            &         cpname, 2034 )
         CALL chkerr( nf90_inq_varid( idfile, 'STATION_TYPE', idcdtyp ), &
            &         cpname, 2036 )
         CALL chkerr( nf90_get_var( idfile, idcdtyp, fbdata%cdtyp), &
            &         cpname, 2038 )
         CALL chkerr( nf90_inq_varid( idfile, 'LONGITUDE', idplam ), &
            &         cpname, 2040 )
         CALL chkerr( nf90_get_var( idfile, idplam, fbdata%plam ), &
            &         cpname, 2042 )
         CALL chkerr( nf90_inq_varid( idfile, 'LATITUDE', idpphi ), &
            &         cpname, 2044 )
         CALL chkerr( nf90_get_var( idfile, idpphi, fbdata%pphi ), &
            &         cpname, 2046 )
         CALL chkerr( nf90_inq_varid( idfile, 'DEPTH', idpdep ), &
            &         cpname, 2048 )
         CALL chkerr( nf90_get_var( idfile, idpdep, fbdata%pdep ), &
            &         cpname, 2050 )
         CALL chkerr( nf90_inq_varid( idfile, 'JULD', idptim ), &
            &         cpname, 2052 )
         CALL chkerr( nf90_get_var( idfile, idptim, fbdata%ptim ), &
            &         cpname, 2054 )
         CALL chkerr( nf90_inq_varid( idfile, 'OBSERVATION_QC', idioqc ), &
            &         cpname, 2056 )
         CALL chkerr( nf90_get_var( idfile, idioqc, fbdata%ioqc ), &
            &         cpname, 2058 )
         CALL chkerr( nf90_inq_varid( idfile, 'OBSERVATION_QC_FLAGS', idioqcf ), &
            &         cpname, 2060 )
         CALL chkerr( nf90_get_var( idfile, idioqcf, fbdata%ioqcf ), &
            &         cpname, 2062 )
         CALL chkerr( nf90_inq_varid( idfile, 'POSITION_QC', idipqc ), &
            &         cpname, 2064 )
         CALL chkerr( nf90_get_var( idfile, idipqc, fbdata%ipqc ), &
            &         cpname, 2066 )
         CALL chkerr( nf90_inq_varid( idfile, 'POSITION_QC_FLAGS', idipqcf ), &
            &         cpname, 2068 )
         CALL chkerr( nf90_get_var( idfile, idipqcf, fbdata%ipqcf ), &
            &         cpname, 2070 )
         CALL chkerr( nf90_inq_varid( idfile, 'DEPTH_QC', ididqc ), &
            &         cpname, 2072 )
         CALL chkerr( nf90_get_var( idfile, ididqc, fbdata%idqc ), &
            &         cpname, 2074 )
         CALL chkerr( nf90_inq_varid( idfile, 'DEPTH_QC_FLAGS', ididqcf ), &
            &         cpname, 2076 )
         CALL chkerr( nf90_get_var( idfile, ididqcf, fbdata%idqcf ), &
            &         cpname, 2078 )
         CALL chkerr( nf90_inq_varid( idfile, 'JULD_QC', iditqc ), &
            &         cpname, 2080 )
         CALL chkerr( nf90_get_var( idfile, iditqc, fbdata%itqc ), &
            &         cpname, 2082 )
         CALL chkerr( nf90_inq_varid( idfile, 'JULD_QC_FLAGS', iditqcf ), &
            &         cpname, 2084 )
         CALL chkerr( nf90_get_var( idfile, iditqcf, fbdata%itqcf ), &
            &         cpname, 2086 )
         CALL chkerr( nf90_inq_varid( idfile, 'ORIGINAL_FILE_INDEX', idkindex ), &
            &         cpname, 2088 )
         CALL chkerr( nf90_get_var( idfile, idkindex, fbdata%kindex ), &
            &         cpname, 2090 )
         
         ! Read netCDF variables for individual variables
         
         DO jv = 1, fbdata%nvar
            
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_OBS'
            CALL chkerr( nf90_inq_varid( idfile, cdtmp, idpob(jv) ), &
               &         cpname, 2098 )
            CALL chkerr( nf90_get_var( idfile, idpob(jv), &
               &                       fbdata%pob(:,:,jv) ), &
               &         cpname, 2101 )
            CALL getvaratt_obfbdata( idfile, idpob(jv), &
               &                     fbdata%coblong(jv), &
               &                     fbdata%cobunit(jv) )
            
            IF ( fbdata%nadd > 0 ) THEN
               DO je = 1, fbdata%nadd
                  WRITE(cdtmp,'(3A)') TRIM(fbdata%cname(jv)),'_',&
                     &                TRIM(fbdata%caddname(je))
                  CALL chkerr( nf90_inq_varid( idfile, cdtmp, idpadd(je,jv) ), &
                     &         cpname, 2111 )
                  CALL chkerr( nf90_get_var( idfile, idpadd(je,jv), &
                     &                       fbdata%padd(:,:,je,jv) ), &
                     &         cpname, 2114 )
                  CALL getvaratt_obfbdata( idfile, idpadd(je,jv), &
                     &                     fbdata%caddlong(je,jv), &
                     &                     fbdata%caddunit(je,jv) )
               END DO
            ENDIF
            
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_QC'
            CALL chkerr( nf90_inq_varid( idfile, cdtmp, idivqc(jv) ), &
            &         cpname, 2123 )
            CALL chkerr( nf90_get_var( idfile, idivqc(jv), &
               &                       fbdata%ivqc(:,jv) ), &
               &         cpname, 2126 )
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_QC_FLAGS'
            CALL chkerr( nf90_inq_varid( idfile, cdtmp, idivqcf(jv) ), &
               &         cpname, 2129 )
            CALL chkerr( nf90_get_var( idfile, idivqcf(jv), &
               &                       fbdata%ivqcf(:,:,jv) ), &
               &         cpname, 2132 )
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_LEVEL_QC'
            CALL chkerr( nf90_inq_varid( idfile, cdtmp, idivlqc(jv) ), &
               &         cpname, 2135 )
            CALL chkerr( nf90_get_var( idfile, idivlqc(jv), &
               &                       fbdata%ivlqc(:,:,jv) ), &
               &         cpname, 2138 )
            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_LEVEL_QC_FLAGS'
            CALL chkerr( nf90_inq_varid( idfile, cdtmp, idivlqcf(jv) ), &
               &         cpname, 2141 )
            CALL chkerr( nf90_get_var( idfile, idivlqcf(jv), &
               &                       fbdata%ivlqcf(:,:,:,jv) ), &
               &         cpname, 2144 )
            IF ( lgrid ) THEN
               WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_IOBSI'
               CALL chkerr( nf90_inq_varid( idfile, cdtmp, idiobsi(jv) ), &
                  &         cpname, 2148 )
               CALL chkerr( nf90_get_var( idfile, idiobsi(jv), &
                  &                       fbdata%iobsi(:,jv) ), &
                  &         cpname, 2151 )
               WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_IOBSJ'
               CALL chkerr( nf90_inq_varid( idfile, cdtmp, idiobsj(jv) ), &
                  &         cpname, 2154 )
               CALL chkerr( nf90_get_var( idfile, idiobsj(jv), &
                  &                       fbdata%iobsj(:,jv) ), &
                  &         cpname, 2157 )
               WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_IOBSK'
               CALL chkerr( nf90_inq_varid( idfile, cdtmp, idiobsk(jv) ), &
                  &         cpname, 2160 )
               CALL chkerr( nf90_get_var( idfile, idiobsk(jv), &
                  &                       fbdata%iobsk(:,:,jv) ), &
                  &         cpname, 2163 )
               WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_GRID'
               CALL chkerr( nf90_inq_varid( idfile, cdtmp, idcgrid(jv) ), &
                  &         cpname, 2166 )
               CALL chkerr( nf90_get_var( idfile, idcgrid(jv), &
                  &                       fbdata%cgrid(jv) ), &
                  &         cpname, 2169 )
            ENDIF
            
         END DO
         
         IF ( fbdata%next > 0 ) THEN
            DO je = 1, fbdata%next
               WRITE(cdtmp,'(A)') TRIM(fbdata%cextname(je))
               CALL chkerr( nf90_inq_varid( idfile, cdtmp, idpext(je) ), &
                  &         cpname, 2178 )
               CALL chkerr( nf90_get_var( idfile, idpext(je), &
                  &                       fbdata%pext(:,:,je) ), &
                  &         cpname, 2181 )
               CALL getvaratt_obfbdata( idfile, idpext(je), &
                  &                     fbdata%cextlong(je), &
                  &                     fbdata%cextunit(je) )
            END DO
         ENDIF

      ELSE ! if no observations only get attributes

         DO jv = 1, fbdata%nvar            

            WRITE(cdtmp,'(2A)') TRIM(fbdata%cname(jv)),'_OBS'
            CALL chkerr( nf90_inq_varid( idfile, cdtmp, idpob(jv) ), &
               &         cpname, 2194 )
            CALL getvaratt_obfbdata( idfile, idpob(jv), &
               &                     fbdata%coblong(jv), &
               &                     fbdata%cobunit(jv) )
            
            IF ( fbdata%nadd > 0 ) THEN
               DO je = 1, fbdata%nadd
                  WRITE(cdtmp,'(3A)') TRIM(fbdata%cname(jv)),'_',&
                     &                TRIM(fbdata%caddname(je))
                  CALL chkerr( nf90_inq_varid( idfile, cdtmp, idpadd(je,jv) ), &
                     &         cpname, 2204 )
                  CALL getvaratt_obfbdata( idfile, idpadd(je,jv), &
                     &                     fbdata%caddlong(je,jv), &
                     &                     fbdata%caddunit(je,jv) )
               END DO
            ENDIF
            
         END DO
         
         IF ( fbdata%next > 0 ) THEN
            DO je = 1, fbdata%next
               WRITE(cdtmp,'(A)') TRIM(fbdata%cextname(je))
               CALL chkerr( nf90_inq_varid( idfile, cdtmp, idpext(je) ), &
                  &         cpname, 2217 )
               CALL getvaratt_obfbdata( idfile, idpext(je), &
                  &                     fbdata%cextlong(je), &
                  &                     fbdata%cextunit(je) )
            END DO
         ENDIF

      ENDIF

      ! Close the file

      CALL chkerr( nf90_close( idfile ), cpname, 2228 )

   END SUBROUTINE read_obfbdata

   SUBROUTINE getvaratt_obfbdata( idfile, idvar, cdlongname, cdunits )
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE putvaratt_obfbdata  ***
      !!
      !! ** Purpose :   Read netcdf attributes for variable
      !!
      !! ** Method  :   
      !!
      !! ** Action  : 
      !!
      !!----------------------------------------------------------------------
      !! * Arguments
      INTEGER :: idfile      ! File netcdf id.
      INTEGER :: idvar       ! Variable netcdf id.
      CHARACTER(len=*) :: cdlongname  ! Long name for variable
      CHARACTER(len=*) :: cdunits     ! Units for variable
      !! * Local variables
      CHARACTER(LEN=18), PARAMETER :: cpname = 'getvaratt_obfbdata'

      CALL chkerr( nf90_get_att( idfile, idvar, 'long_name', &
         &                       cdlongname ), &
         &                       cpname, 2253 )

      CALL chkerr( nf90_get_att( idfile, idvar, 'units', &
         &                       cdunits ), &
         &                       cpname, 2257 )

   END SUBROUTINE getvaratt_obfbdata

END MODULE obs_fbm
