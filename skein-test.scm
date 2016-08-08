
(require easy
	 skein
	 test
	 (cj-env-2 repeat))


(TEST
 > (skein:digest "")
 "bc5b4c50925519c290cc634277ae3d6257212395cba733bbad37a4af0fa06af41fca7903d06564fea7a2d3730dbdb80c1f85562dfcc070334ea4d1d9e72cba7a"
 > (skein:digest "foo")
 "fd8956898113510180aa4658e6c0ac85bd74fb47f4a4ba264a6b705d7a8e8526756e75aecda12cff4f1aca1a4c2830fbf57f458012a66b2b15a3dd7d251690a7"
 > (skein:digest "The quick brown fox jumps over the lazy dog")
 "94c2ae036dba8783d0b3f7d6cc111ff810702f5c77707999be7e1c9486ff238a7044de734293147359b4ac7e1d09cd247c351d69826b78dcddd951f0ef912713"
 > (skein:digest "The quick brown fox jumps over the lazy dog.")
 "658223cb3d69b5e76e3588ca63feffba0dc2ead38a95d0650564f2a39da8e83fbb42c9d6ad9e03fbfde8a25a880357d457dbd6f74cbcb5e728979577dbce5436"
 )

;; don't understand how comes that KAT_MCT/ShortMsgKAT_512.txt or any
;; other files in the reference implementation don't have
;; those. Wikipedia confirms the above.


;; Some benchmarking

(def (bench-skein msg n)
     (time (repeat n
		   (skein:digest msg))))

