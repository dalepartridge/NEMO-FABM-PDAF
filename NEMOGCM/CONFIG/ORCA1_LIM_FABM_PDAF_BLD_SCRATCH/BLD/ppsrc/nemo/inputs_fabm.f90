












MODULE inputs_fabm
   !!======================================================================
   !!                         ***  MODULE inputs_fabm  ***
   !! TOP :   Input module of the FABM tracers
   !!======================================================================

   !!----------------------------------------------------------------------
   !!   'key_fabm'                                               FABM tracers
   !!----------------------------------------------------------------------
   !! initialize_inputs       : initialize input structures
   !! update_inputs : update 2D input fields
   !! trc_rnf_fabm : update river data
   !!----------------------------------------------------------------------
   USE par_trc
   USE oce_trc
   USE trc
   USE iom
   USE fldread
   USE par_fabm
   USE fabm, only: type_fabm_horizontal_variable_id

   IMPLICIT NONE

   !!----------------------------------------------------------------------
   !!                   ***  vectopt_loop_substitute  ***
   !!----------------------------------------------------------------------
   !! ** purpose :   substitute the inner loop start/end indices with CPP macro
   !!                allow unrolling of do-loop (useful with vector processors)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO Consortium (2014)
   !! $Id$
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

   PRIVATE

   PUBLIC initialize_inputs
   PUBLIC link_inputs
   PUBLIC update_inputs
   PUBLIC trc_rnf_fabm


   TYPE, PUBLIC :: type_input_variable
      TYPE(FLD), ALLOCATABLE, DIMENSION(:) :: sf
      INTEGER                              :: ntimes
   END TYPE

   TYPE, PUBLIC, EXTENDS(type_input_variable) :: type_input_data
      TYPE(type_fabm_horizontal_variable_id) :: horizontal_id
      TYPE(type_input_data), POINTER         :: next => null()
   END TYPE
   TYPE (type_input_data), POINTER, PUBLIC :: first_input_data => NULL()

   TYPE, PUBLIC, EXTENDS(type_input_variable):: type_river_data
      INTEGER   :: jp_pos=0 ! position of linked state variable in trc fields
      REAL(wp) :: rn_trrnfac=1._wp ! unit conversion factor
      TYPE(type_river_data), POINTER   :: next => null()
   END TYPE
   TYPE (type_river_data), POINTER, PUBLIC :: first_river_data => NULL()

   CONTAINS

     SUBROUTINE initialize_inputs
        TYPE(FLD_N)        :: sn, sn_empty
        CHARACTER(LEN=256) :: name
        REAL(wp) :: rfac
        NAMELIST /variable/ name,sn
        NAMELIST /riverdata/ name,sn,rfac
        LOGICAL :: l_ext
        INTEGER :: num, ierr, nmlunit
        TYPE (type_input_data),POINTER :: input_data
        TYPE (type_river_data),POINTER :: river_data
        INTEGER :: jn
        INTEGER , PARAMETER :: nbtimes = 366  !: maximum number of times record in a file
        REAL(wp), DIMENSION(nbtimes) :: zsteps

        ! Check if fabm_input.nml exists - if not, do nothing and return.
        INQUIRE( FILE='fabm_input.nml', EXIST=l_ext )
        IF (.NOT.l_ext) return

        ! Open fabm_input.nml
        CALL ctl_opn( nmlunit, 'fabm_input.nml', 'OLD', 'FORMATTED', 'SEQUENTIAL', -1, num, .FALSE. )

        ! Read any number of "variable" namelists
        DO
           ! Initialize namelist variables
           name = ''
           sn = sn_empty

           ! Read the namelist
           READ(nmlunit,nml=variable,err=98,end=99)

           ! Transfer namelist settings to new input_data object
           ALLOCATE(input_data, STAT=ierr)
           IF( ierr > 0 ) CALL ctl_stop( 'STOP', 'inputs_fabm:initialize_inputs: unable to allocate input_data object for variable '//TRIM(name) )
           input_data%horizontal_id = model%get_horizontal_variable_id(name)
           IF (.NOT.model%is_variable_used(input_data%horizontal_id)) THEN
              ! This variable was not found among FABM's horizontal variables (at least, those that are read by one or more FABM modules)
              CALL ctl_stop('STOP', 'inputs_fabm:initialize_inputs: variable "'//TRIM(name)//'" was not found among horizontal FABM variables.')
           END IF
           ALLOCATE(input_data%sf(1), STAT=ierr)
           IF( ierr > 0 ) CALL ctl_stop( 'STOP', 'inputs_fabm:initialize_inputs: unable to allocate sf structure for variable '//TRIM(name) )
           CALL fld_fill(input_data%sf, (/sn/), '', 'inputs_fabm:initialize_inputs', 'FABM variable '//TRIM(name), 'variable' )
           ALLOCATE( input_data%sf(1)%fnow(jpi,jpj,1)   )
           IF( sn%ln_tint ) ALLOCATE( input_data%sf(1)%fdta(jpi,jpj,1,2) )

           ! Get number of record in file (if there is only one, we will read data
           ! only at the very first time step)
           CALL fld_clopn( input_data%sf(1) )
           CALL iom_gettime( input_data%sf(1)%num, zsteps, kntime=input_data%ntimes)
           CALL iom_close( input_data%sf(1)%num )

           ! Prepend new input variable to list.
           input_data%next => first_input_data
           first_input_data => input_data
        END DO

  98    CALL ctl_stop('STOP', 'inputs_fabm:initialize_inputs: unable to read namelist "riverdata"')

  99    REWIND(nmlunit)

        ! Read any number of "riverdata" namelists
        DO
           ! Initialize namelist variables
           name = ''
           sn = sn_empty
           rfac = 1._wp

           ! Read the namelist
           READ(nmlunit,nml=riverdata,err=198,end=199)

           ! Transfer namelist settings to new river_data object
           ALLOCATE(river_data, STAT=ierr)
           IF( ierr > 0 ) CALL ctl_stop( 'STOP', 'inputs_fabm:initialize_inputs: unable to allocate river_data object for variable '//TRIM(name) )
           ! Check if river data name is in FABM states and
           ! provide NEMO with position of the respective state variable
           ! within tracer field
           DO jn=1,jp_fabm
             IF (TRIM(name) == TRIM(model%interior_state_variables(jn)%name)) THEN
               river_data%jp_pos = jp_fabm_m1+jn
             END IF
           END DO
           IF (river_data%jp_pos == 0) THEN
             ! This variable was not found among FABM's state variables
             ! passed to NEMO!
             CALL ctl_stop('STOP', 'inputs_fabm:initialize_inputs: variable "'//TRIM(name)//'" was not found among FABM state variables.')
           END IF

           ALLOCATE(river_data%sf(1), STAT=ierr)
           IF( ierr > 0 ) CALL ctl_stop( 'STOP', 'inputs_fabm:initialize_inputs: unable to allocate sf structure for variable '//TRIM(name) )
           CALL fld_fill(river_data%sf, (/sn/), '', 'inputs_fabm:initialize_inputs', 'FABM variable '//TRIM(name), 'riverdata' )
           ALLOCATE( river_data%sf(1)%fnow(jpi,jpj,1)   )
           IF( sn%ln_tint ) ALLOCATE( river_data%sf(1)%fdta(jpi,jpj,1,2) )

           ! Load unit conversion factor:
           river_data%rn_trrnfac=rfac

           ! Get number of record in file (if there is only one, we will read data
           ! only at the very first time step)
           CALL fld_clopn( river_data%sf(1) )
           CALL iom_gettime( river_data%sf(1)%num, zsteps, kntime=river_data%ntimes)
           CALL iom_close( river_data%sf(1)%num )

           ! Prepend new input variable to list.
           river_data%next => first_river_data
           first_river_data => river_data
        END DO

  198   CALL ctl_stop('STOP', 'inputs_fabm:initialize_inputs: unable to read namelist "riverdata"')

  199   RETURN

     END SUBROUTINE initialize_inputs

     SUBROUTINE link_inputs
      TYPE (type_input_data),POINTER :: input_data

      input_data => first_input_data
      DO WHILE (ASSOCIATED(input_data))
         ! Provide FABM with pointer to field that will receive prescribed data.
         ! NB source=data_source_user guarantees that the prescribed data takes priority over any data FABM may already have for that variable.
         CALL model%link_horizontal_data(input_data%horizontal_id,input_data%sf(1)%fnow(:,:,1),source=data_source_user)
         input_data => input_data%next
      END DO

   END SUBROUTINE link_inputs

   SUBROUTINE update_inputs( kt , l_write)
      INTEGER, INTENT(IN) :: kt
      LOGICAL, INTENT(IN), OPTIONAL :: l_write
      TYPE (type_input_data),POINTER :: input_data
      TYPE (type_river_data),POINTER :: river_data

      input_data => first_input_data
      DO WHILE (ASSOCIATED(input_data))
         IF( kt == nit000 .OR. ( kt /= nit000 .AND. input_data%ntimes > 1 ) ) CALL fld_read( kt, 1, input_data%sf )
         input_data => input_data%next
      END DO

      river_data => first_river_data
      DO WHILE (ASSOCIATED(river_data))
         IF( kt == nit000 .OR. ( kt /= nit000 .AND. river_data%ntimes > 1 ) ) CALL fld_read( kt, 1, river_data%sf )
         river_data => river_data%next
      END DO

   END SUBROUTINE update_inputs

   SUBROUTINE trc_rnf_fabm( kt )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE trc_rnf_fabm  ***
      !!
      !! ** Purpose :   Add river loadings of biogeochemistry to states
      !!
      !! ** Action  :   tra (sms) updated with loadings at time-step kt
      !!
      !! This routines assumes river loadings to be given in
      !! state variable units * m3 / sec
      !!--------------------------------------------------------------------

      INTEGER, INTENT(in) ::   kt          ! ocean time step
      REAL(wp) :: zcoef
      INTEGER :: ji,jj,jk
      !
      TYPE (type_river_data),POINTER :: river_data

      river_data => first_river_data
      DO WHILE (ASSOCIATED(river_data))
        IF( kt == nit000 .OR. ( kt /= nit000 ) ) THEN
            DO jj = 2, jpjm1
              DO ji = 2, jpim1
                ! convert units and divide by surface area
                ! loading / cell volume * vertical fraction of riverload
                ! dtrc / dt (river) = riverload / e1e2t / e3t * e3t * h_rnf
                !                    = riverload / e1e2t / h_rnf
                zcoef = river_data%rn_trrnfac / e1e2t(ji,jj) / h_rnf(ji,jj)
                DO jk = 1,nk_rnf(ji,jj)
                  ! Add river loadings
                  if (river_data%rn_trrnfac>=0) then
                    tra(ji,jj,jk,river_data%jp_pos) = tra(ji,jj,jk,river_data%jp_pos) + river_data%sf(1)%fnow(ji,jj,1)*zcoef
                  else
                    !this is for the no river dilution option, where we give the runoff as riverload and we multiply by the current concentration
                    ! no need to use the full zcoeff because the run off is already surface specific, 1000. is to convert kg freshwater to m3
                    tra(ji,jj,jk,river_data%jp_pos) = tra(ji,jj,jk,river_data%jp_pos) + river_data%sf(1)%fnow(ji,jj,1)/1000._wp*trn(ji,jj,jk,river_data%jp_pos) / h_rnf(ji,jj)
                  endif
                END DO
              END DO
            END DO
        END IF
        river_data => river_data%next
      END DO

   END SUBROUTINE trc_rnf_fabm
   !!======================================================================
END MODULE inputs_fabm
