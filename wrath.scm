(define-module (wrath)
  #:use-module (base-system)
  #:use-module (gnu))

(operating-system
 (inherit base-operating-system)
 (host-name "wrath")

 (keyboard-layout (keyboard-layout "us" #:model "thinkpad"))

 (mapped-devices
  (list (mapped-device
         (source
          (uuid "6773b52e-1496-407e-b1d8-9a2ac7f7820f"))
         (target "system-root")
         (type luks-device-mapping))
        (mapped-device
         (source
          (uuid "08123a90-d66b-41ff-8f2c-4435292f7818"))
         (target "crypthome")
         (type luks-device-mapping))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/mapper/system-root")
          (type "ext4")
          (dependencies mapped-devices))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "4B6C-4B80" 'fat32))
          (type "vfat"))
         (file-syste
          (mount-point "/home")
          (device "/dev/mapper/crypthome")
          (type "ext4")
          (dependencies mapped-devices))
         
         %base-file-systems)))
