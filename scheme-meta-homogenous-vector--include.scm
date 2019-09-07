(define (uvector? v)
  (or (u8vector? v)
      (u16vector? v)
      (u32vector? v)
      (u64vector? v)))

(define (svector? v)
  (or (s8vector? v)
      (s16vector? v)
      (s32vector? v)
      (s64vector? v)))

(define (fvector? v)
  (or (f32vector? v)
      (f64vector? v)))

(define (homogenous-vector? v)
  (or (uvector? v)
      (svector? v)
      (fvector? v)))

