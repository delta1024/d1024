(list
 ;; (channel
 ;;  (name 'd1024)
 ;;  (url "https://github.com/delta1024/d1024.git")
 ;;  (introduction
 ;;   (make-channel-introduction
 ;;    "1dc0f4bba185b59833d815222c67034e91fe2588"
 ;;    (openpgp-fingerprint
 ;;     "F532 5D13 813D 82F0 D253  EEAB CA48 C392 F00A 4B91"))))
 (channel
  (name 'flat)
  (url "https://github.com/flatwhatson/guix-channel.git")
  (branch "master")
  (commit
   "cf23f523afc611c2b35debf015d219c6f918337f")
  (introduction
   (make-channel-introduction
    "33f86a4b48205c0dc19d7c036c85393f0766f806"
    (openpgp-fingerprint
     "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (branch "master")
  (commit
   "e068ec9f82d7cba4b319d006ccbaddf0c57354ee")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit
   "4c0b9a86521a6d06c895b41e62c254da83feff7a")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (commit
   "43990c6a292b5958c2002a189fbf3b2ee782ba09")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
