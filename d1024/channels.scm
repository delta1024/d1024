(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "bbb0 2ddf 2cea f6a8 0d1d  e643 a2a0 6df2 a33a 54fa"))))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2a39 3fff 68f4 ef7a 3d29  12af 6f51 20a0 22fb b2d5"))))
      (channel
       (name 'rde)
       (url "https://git.sr.ht/~abcdw/rde")
       (introduction
        (make-channel-introduction
         "257cebd587b66e4d865b3537a9a88cccd7107c95"
         (openpgp-fingerprint
          "2841 9ac6 5038 7440 c7e9  2ffa 2208 d209 58c1 deb0"))))
      (channel
       (name 'flat)
       (url "https://github.com/flatwhatson/guix-channel.git")
       (introduction
        (make-channel-introduction
         "33f86a4b48205c0dc19d7c036c85393f0766f806"
         (openpgp-fingerprint
          "736a c00e 1254 378b a982  7af6 9dbe 8265 81b6 4490")))))