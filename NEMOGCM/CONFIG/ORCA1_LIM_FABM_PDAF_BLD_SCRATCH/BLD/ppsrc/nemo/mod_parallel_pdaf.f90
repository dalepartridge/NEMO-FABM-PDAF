












MODULE mod_parallel_pdaf

! This module provides variables for the MPI parallelization
! to be shared between model-related routines. There are variables
! that are used in the model even without PDAF, and additional variables
! that are only used if data assimilaion with PDAF is performed.
! The initialization of communicators for execution with PDAF is
! performed in init_parallel_pdaf.

   USE mod_kind_pdaf

   IMPLICIT NONE
   SAVE

   INCLUDE 'mpif.h'

   ! MPI communicator for model tasks
   INTEGER :: COMM_model
   ! Rank and size in COMM_model
   INTEGER :: mype_model, npes_model
   ! Communicator for entire ensemble
   INTEGER :: COMM_ensemble
   ! Rank and size in COMM_ensemble
   INTEGER :: mype_ens, npes_ens

   ! Number of parallel model tasks
   INTEGER :: n_modeltasks = 1
   ! MPI communicator for filter PEs
   INTEGER :: COMM_filter
   ! rank and size in COMM_filter
   INTEGER :: mype_filter, npes_filter
   ! MPI communicator for coupling filter and model
   INTEGER :: COMM_couple
   ! rank and size in COMM_couple
   INTEGER :: mype_couple, npes_couple
   ! Whether we are on a PE in a COMM_filter
   LOGICAL :: filterpe
   ! Index of my model task (1,...,n_modeltasks)
   INTEGER :: task_id
   ! Error flag for MPI
   INTEGER :: MPIerr
   ! Status array for MPI
   INTEGER :: MPIstatus(MPI_STATUS_SIZE)
   ! Option for screen output
   INTEGER :: screen_parallel = 1
   ! # PEs per ensemble
   INTEGER, ALLOCATABLE :: local_npes_model(:)

CONTAINS

   SUBROUTINE abort_parallel()

      CALL MPI_Abort(MPI_COMM_WORLD, 1, MPIerr)

   END SUBROUTINE abort_parallel

   SUBROUTINE init_parallel_pdaf(screen, mpi_comm)

     ! Split the MPI communicator initialised by XIOS into MODEL,
     ! FILTER and COUPLE communicators, return MODEL communicator.
     !
     ! **Calling Sequence**
     ! *Called from:* `lib_mpp.F90`
     ! *Calls:* `MPI_Comm_size`
     ! *Calls:* `MPI_Comm_rank`
     ! *Calls:* `MPI_Comm_split`
     ! *Calls:* `MPI_Barrier`     

      ! Whether screen information is shown
      INTEGER, INTENT(in)    :: screen
      ! Communicator after XIOS splitting
      INTEGER, INTENT(inout) :: mpi_comm

      ! Counters
      INTEGER :: i, j
      ! Index of PE
      INTEGER :: pe_index
      ! Variables for communicator-splitting
      INTEGER :: my_color, color_couple
      ! Number of model tasks
      INTEGER :: tasks
      ! namelist file
      CHARACTER(lc) :: nmlfile

      ! Number of ensemble members, supplied by PDAF namelist
      NAMELIST /tasks_nml/ tasks

      ! Read namelist for number of model tasks
      nmlfile = 'namelist.pdaf'

      OPEN (20, file=nmlfile)
      READ (20, NML=tasks_nml)
      CLOSE (20)

      n_modeltasks = tasks

      ! ***              COMM_ENSEMBLE                ***
      ! *** Generate communicator for ensemble runs   ***
      ! *** only used to generate model communicators ***

      COMM_ensemble = mpi_comm
      CALL MPI_Comm_Size(COMM_ensemble, npes_ens, MPIerr)
      CALL MPI_Comm_Rank(COMM_ensemble, mype_ens, MPIerr)

      ! Initialize communicators for ensemble evaluations
      IF (mype_ens == 0) THEN
         WRITE (*, '(/1x, a)') 'Initialize communicators for assimilation with PDAF'
      END IF

      ! Store # PEs per ensemble member. Used for info on PE 0 and for
      ! generation of model communicators on other PEs
      ALLOCATE (local_npes_model(n_modeltasks))
      local_npes_model = FLOOR(REAL(npes_ens)/REAL(n_modeltasks))

      DO i = 1, (npes_ens - n_modeltasks*local_npes_model(1))
         local_npes_model(i) = local_npes_model(i) + 1
      END DO

      ! ***              COMM_MODEL               ***
      ! *** Generate communicators for model runs ***

      pe_index = 0
      doens1: DO i = 1, n_modeltasks
         DO j = 1, local_npes_model(i)
            IF (mype_ens == pe_index) THEN
               task_id = i
               EXIT doens1
            END IF
            pe_index = pe_index + 1
         END DO
      END DO doens1

      CALL MPI_Comm_split(COMM_ensemble, task_id, mype_ens, &
                          COMM_model, MPIerr)

      ! Re-initialize PE information according to model communicator
      CALL MPI_Comm_Size(COMM_model, npes_model, MPIerr)
      CALL MPI_Comm_Rank(COMM_model, mype_model, MPIerr)

      IF (screen > 1) then
         WRITE (*, *) 'MODEL: mype(w)= ', mype_ens, '; model task: ', task_id, &
            '; mype(m)= ', mype_model, '; npes(m)= ', npes_model
      END IF

      ! Init flag FILTERPE (all PEs of model task 1)
      IF (task_id == 1) THEN
         filterpe = .TRUE.
      ELSE
         filterpe = .FALSE.
      END IF

      ! ***         COMM_FILTER                 ***
      ! *** Generate communicator for filter    ***

      IF (filterpe) THEN
         my_color = task_id
      ELSE
         my_color = MPI_UNDEFINED
      END IF

      CALL MPI_Comm_split(COMM_ensemble, my_color, mype_ens, &
                          COMM_filter, MPIerr)

      ! Initialize PE information according to filter communicator
      IF (filterpe) THEN
         CALL MPI_Comm_Size(COMM_filter, npes_filter, MPIerr)
         CALL MPI_Comm_Rank(COMM_filter, mype_filter, MPIerr)
      END IF

      ! ***              COMM_COUPLE                 ***
      ! *** Generate communicators for communication ***
      ! *** between model and filter PEs             ***

      color_couple = mype_model + 1

      CALL MPI_Comm_split(COMM_ensemble, color_couple, mype_ens, &
                          COMM_couple, MPIerr)

      ! Initialize PE information according to coupling communicator
      CALL MPI_Comm_Size(COMM_couple, npes_couple, MPIerr)
      CALL MPI_Comm_Rank(COMM_couple, mype_couple, MPIerr)

      IF (screen > 0) THEN
         IF (mype_ens == 0) THEN
            WRITE (*, '(/18x, a)') 'PE configuration:'
            WRITE (*, '(2x, a6, a9, a10, a14, a13, /2x, a5, a9, a7, a7, a7, a7, a7, /2x, a)') &
               'world', 'filter', 'model', 'couple', 'filterPE', &
               'rank', 'rank', 'task', 'rank', 'task', 'rank', 'T/F', &
               '----------------------------------------------------------'
         END IF
         CALL MPI_Barrier(COMM_ensemble, MPIerr)
         IF (task_id == 1) THEN
            WRITE (*, '(2x, i4, 4x, i4, 4x, i3, 4x, i3, 4x, i3, 4x, i3, 5x, l3)') &
               mype_ens, mype_filter, task_id, mype_model, color_couple, &
               mype_couple, filterpe
         END IF
         IF (task_id > 1) THEN
            WRITE (*, '(2x, i4, 12x, i3, 4x, i3, 4x, i3, 4x, i3, 5x, l3)') &
               mype_ens, task_id, mype_model, color_couple, mype_couple, filterpe
         END IF
         CALL MPI_Barrier(COMM_ensemble, MPIerr)

         IF (mype_ens == 0) WRITE (*, '(/a)') ''

      END IF

      ! ****************************************************
      ! *** Re-initialize model equivalent to COMM_model ***
      ! ****************************************************

      mpi_comm = COMM_model

   END SUBROUTINE init_parallel_pdaf

END MODULE mod_parallel_pdaf
