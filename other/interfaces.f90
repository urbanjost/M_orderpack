Interface
   Subroutine CTRPER (XVALT, PCLS)
      Real, Dimension (:), Intent (Inout) :: XVALT
      Real, Intent (In) :: PCLS
   End Subroutine CTRPER
End Interface
Interface
   Function FNDNTH (XDONT, NORD)
      Real, Dimension (:), Intent (In) :: XDONT
      Real (Kind(XDONT)) :: FNDNTH
      Integer, Intent (In) :: NORD
   End Function FNDNTH
End Interface
Interface
   Subroutine INDMED (XDONT, INDM)
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Intent (Out) :: INDM
   End Subroutine INDMED
End Interface
Interface
   Function INDNTH (XDONT, NORD)
      Integer :: INDNTH
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Intent (In) :: NORD
   End Function INDNTH
End Interface
Interface
   Subroutine INSPAR (XDONT, NORD)
      Real, Dimension (:), Intent (Inout) :: XDONT
      Integer, Intent (In) :: NORD
   End Subroutine INSPAR
End Interface
Interface
   Subroutine INSSOR (XDONT)
      Real, Dimension (:), Intent (Inout) :: XDONT
   End Subroutine INSSOR
End Interface
Interface
   Subroutine MRGREF (XVALT, IRNGT)
      Real, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
   End Subroutine MRGREF
End Interface
Interface
   Subroutine MRGRNK (XVALT, IRNGT)
      Real, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
   End Subroutine MRGRNK
End Interface
Interface
   Subroutine MULCNT (XVALT, IMULT)
      Real, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (Size(XVALT)), Intent (Out) :: IMULT
   End Subroutine MULCNT
End Interface
Interface
   Subroutine REFPAR (XDONT, IRNGT, NORD)
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
   End Subroutine REFPAR
End Interface
Interface
   Subroutine REFSOR (XDONT)
      Real, Dimension (:), Intent (Inout) :: XDONT
   End Subroutine REFSOR
End Interface
Interface
   Subroutine RINPAR (XDONT, IRNGT, NORD)
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
   End Subroutine RINPAR
End Interface
Interface
   Subroutine RNKPAR (XDONT, IRNGT, NORD)
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
   End Subroutine RNKPAR
End Interface
Interface
   Subroutine UNIINV (XVALT, IGOEST)
      Real, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IGOEST
   End Subroutine UNIINV
End Interface
Interface
   Subroutine UNIPAR (XDONT, IRNGT, NORD)
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (Inout) :: NORD
   End Subroutine UNIPAR
End Interface
Interface
   Subroutine UNIRNK (XVALT, IRNGT, NUNI)
      Real, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (Out) :: NUNI
   End Subroutine UNIRNK
End Interface
Interface
   Subroutine UNISTA (XVALT, NUNI)
      Real, Dimension (:), Intent (Inout) :: XVALT
      Integer, Intent (Out) :: NUNI
   End Subroutine UNISTA
End Interface
Interface
   Function VALMED (XVALT)
      Real, Dimension (:), Intent (In) :: XVALT
      Real (Kind(XVALT)) :: VALMED
   End Function VALMED
End Interface
Interface
   Function VALNTH (XDONT, NORD)
      Real, Dimension (:), Intent (In) :: XDONT
      Real (Kind(XDONT)) :: VALNTH
      Integer, Intent (In) :: NORD
   End Function VALNTH
End Interface
