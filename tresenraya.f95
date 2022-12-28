program tres_en_raya

  implicit none
    
    character(len=21) :: jugador1, jugador2                !nombre de los jugadores almacenados en una variable cada uno
    character(len=20), dimension(2)  :: v_jugador          !vector de 2 dimensiones que almacena los nombres de ambos jugadores y no exceden 20 caracteres
    real :: jug_empieza                                    !numero generado aleatoriamente para ver quien empieza primero
    integer :: A(3,3)                                      !Tablero, matriz de ceros que se irá modificando a medida que avance la partida
    integer :: i,j,k                                       !contador para los loop
    integer :: fila,columna,contador_movimientos           !fila/columna que introduce el usuario y contador de movimientos
    integer :: flag_diagonal1, flag_Diagonal2, flag_colfil !comprobadores que cambian a uno cuando se cumple la condicion del tres en raya en esa dimension
    logical, dimension(3) :: tr_filas, tr_columnas         !vectores que almacenaran los true y false de si una fila o columna entera esta rellenas o de 1 o de 2
    character ::  B(3,3)                                   !tablero que rellenaremos con X y con O y en los huecos con -, TABLERO DE CHARACTERS
    character(len=40) :: format                            !formato de la matriz de characters a la hora de imprimirla
    
    A = 0                           !llena la matriz de numeros con 0 (valor inicial)
    !flags valen 0 cuando no se cumplen y 1 cuando se cumplen
    flag_diagonal1=0
    flag_diagonal2=0
    flag_colfil=0                  
    B='-'                          !llena los elementos de la matriz caracteres con guiones
    format = '(2X,A1,2X,1A,2X,1A)' !deja dos espacios entre los elementos character de la matriz(A)

    !inicio del juego. Se explica su funcionamiento y se le pide los nombres a los jugadores.

    write(*,'(A39,/)') ' ============= TRES EN RAYA ============'
    write(*,'(1X,A58,/)') 'Hecho por David Jimenez y Juan Martinez'
    write(*,*) '============= INSTRUCCIONES ============='
    write(*,*)  'El juego consiste en conseguir conectar 3 puntos, en el tablero de 9 espacios, seguidos ya sea'
    write(*,*)  'horizontalmente, verticalmente o en las diagonales. Por turnos se van colocando los o y x de cada uno'
    write(*,*)  'hasta conseguir el 3 en raya. NOTA: su nombre no debe exceder los 20 caracteres'
    
    write(*,*) '=========== INTRODUZCA SU NOMBRE ==========='

  2 write(*,*) 'Jugador 1:'; read(*,*) jugador1; !pide y guarada el nombre del primer jugador

        if (len_Trim(jugador1)>20) then          !coge la longitud del nombre del jugador sin espacios y si eso es mayor que 20 entonces salta el error

          write(*,*) "Por favor, escriba un nombre que no exceda los 20 caracteres"
          goto 2

        endif

  3 write(*,*) 'Jugador 2:'; read(*,*) jugador2; !pide y guarda el nombre del segundo jugador

        if (len_trim(jugador2)>20) then

          write(*,*) "Por favor, escriba un nombre que no exceda los 20 caracteres"
          goto 3

        endif
      
    ! Inicio del juego

    write(*,*) '=========== JUGADOR QUE COMIENZA ==========='
    
    call random_number(jug_empieza)       !llama un numero aleatorio de 0 a 1
    jug_empieza=nint(jug_empieza)         !redondea el numero decimal al entero mas cercano. de 0.49 para bajo es 0 y de 0.5 para arriba es 1

        if (jug_empieza==0) then          !si sale 0, comienza el jugador 1

            write(*,*) 'Empieza el jugador 1: ', jugador1
            v_jugador(1)=jugador1         !colacamos en primera posicion del vector de jugadores a jugador 1
            v_jugador(2)=jugador2

        else                              !si sale 1, comienza el jugador 2

            write(*,*) 'Empieza el jugador 2: ', jugador2
            v_jugador(1)=jugador2         !colocamos en la primera posicion del vector jugadores al jugador 2
            v_jugador(2)=jugador1

        endif

  ! Comienzo y evolución de la partida

  do contador_movimientos = 1, 100 ! Establecemos un contador de movimientos a través del cual evolucionará el juego

      write(*,'(/,A30,1X,I2,A12)') "============ MOVIMIENTO NUMERO",contador_movimientos," ============"

        if (mod(contador_movimientos,2)/=0) then

             write(*,*) "Turno de ", v_jugador(1) 

        else 
 
             write(*,*) "Turno de ", v_jugador(2)

        endif

      write(*,*) "TABLERO:"

        do i = 1,3
      
          write(*,fmt=format) (B(i,j), j =1,3)    !escribe la matriz de caracteres vacia antes del juego

        enddo

      write(*,*) "Indique la posicion del tablero a ocupar. Para ello, indique la fila y la columna en la que quiere situarse."

      1    write(*,*) "Fila:"
           read(*,*) fila
           write(*,*) "Columna:"
           read(*,*) columna

if (0<fila .and. fila<=3 .and. 0<columna .and. columna<=3 .and. A(fila,columna)==0) then

       if (mod(contador_movimientos,2)/=0) then

           A(fila,columna) = 1                 

       else 

           A(fila,columna) = 2
        
       endif

       !CAMBIO DE NUMERO A LETRAS
       do i=1,3                         !Recorre filas
        
        do j=1,3                        !Recorre columnas
          
          if (A(i,j)==1) B(i,j)='X'     !si la matriz de numeros tiene un 1 en esa posicion, la B de letras tendra una X
          if (A(i,j)==2) B(i,j)='O'     !si A tiene un 2, en esa posicion tendrá la B un O
          if (A(i,j)==0) B(i,j)='-'     !si A tiene un 0, en esa posicion, tendrá un gion en la B
        
        enddo

       enddo

    do i =1,3
    
        write(*,fmt=format) (B(i,j), j =1,3)    !escribe la matriz de caracteres con lo que puso el usuario esa ronda

    enddo


    !COMPROBACIÓN DEL TRES EN RAYA: utiliza la matriz de numeros para comprobarlo
    !COMPROBACION DIAGONAL: si todos los elementos de la diagonal son iguales y distintos a 0 entonces es tres en raya y se cumple la flag
    
    if (A(1,1)==A(2,2).AND.A(2,2)==A(3,3).AND. A(1,1)/=0 .AND. A(2,2)/=0 .AND. A(3,3)/=0) flag_diagonal1=1
    if (A(1,3)==A(2,2).AND.A(2,2)==A(3,1).AND. A(1,3)/=0 .AND. A(2,2)/=0 .AND. A(3,1)/=0) flag_diagonal2=1

    
    !COMPROBACION FILAS/COLUMNAS: si las filas o columnas estan llenas enteras del mismo numero, el ALL pondrá un true en el vector logical para el tres en raya
    
    tr_filas=.false.
    tr_columnas=.false.
    if (mod(contador_movimientos,2)/=0) then    !si el turno es impar, comprueba que los 1 de la matriz A esten o no en tres en raya
      
      tr_filas=ALL(A==1,1)
      tr_columnas=ALL(A==1,2)

      do k=1,3
        
        if ((tr_filas(k) .eqv. .true.).or.(tr_columnas(k) .eqv. .true.)) then !si alguna de las columnas esta llena de 1 (por tanto es true), es tres en raya
          
          flag_colfil=1
          exit

        else
          
          flag_colfil=0

        endif

      enddo
    
    else 
      
      tr_filas=ALL(A==2,1)
      tr_columnas=ALL(A==2,2)

      do k=1,3

        if ((tr_filas(k) .eqv. .true.).or.(tr_columnas(k) .eqv. .true.)) then !si alguna de las columnas esta llena de 2 (por tanto es true), es tres en raya
          
          flag_colfil=1
          exit

        else

          flag_Colfil=0

        endif
        
      enddo
    
    endif

    !COMRUEBA CONDICIONES: si alguna se cumple para el tres en raya, el jugador de esa ronda gana la partida

    if (flag_colfil==1 .or. flag_diagonal1==1 .or. flag_diagonal2==1) then

      if (mod(contador_movimientos,2)/=0) then

        write(*,fmt='(A,A,A,1X,I1)') 'Y el ganador es... ',trim(v_jugador(1)),'!!!. Turnos tomados:',contador_movimientos
        stop

      else

        write(*,fmt='(A,A,A,1X,I1)') 'Y el ganador es... ',trim(v_jugador(2)),'!!!. Turnos tomados:',contador_movimientos
        stop

      endif

     !COMPROBACION DE EMPATE: si llegamos a un empate donde no se cumplen las condiciones tras 9 movimientos
     
    elseif (contador_movimientos==9) then     

      write(*,*) 'Empate. No ha ganado ningun jugador al llegar al numero maximo de turnos.'
      stop 
    
    endif
              
else if (3<fila .or. fila<=0 .or. 3<columna .or. columna<=0) then

  write(*,*) "Introduzca un numero de fila y/o columna valido"
  goto 1

else 

  write(*,*) "Esa posicion del tablero ya esta ocupada por tu adversario. "
  write(*,*) "Introduzca una posicion del tablero sin ocupar"
  goto 1

endif

enddo

    
end program tres_en_raya