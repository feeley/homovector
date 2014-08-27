;;;============================================================================

;;; File: "homovector.sld"

;;; Copyright (c) 2006-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on homogeneous vectors.

(define-library (http://github.com/feeley/homovector)

  (export ISO-8859-1-substring->u8vector
          ISO-8859-1-string->u8vector
          subu8vector->ISO-8859-1-string
          u8vector->ISO-8859-1-string

          hex-substring->u8vector
          hex-string->u8vector
          subu8vector->hex-string
          u8vector->hex-string)

  (import (gambit))

  (include "homovector.scm")
)

;;;============================================================================
