(define-module (d1024 services emacs org capture))
(define-public org-capture
  "\
(setq org-capture-templates
      '((\"t\" \"TODO\")
        (\"tg\" \"General\" entry (file+olp \"~/Documents/org/Task.org\" \"General\")
         \"* TODO %^{Title}\n %?\")
        (\"th\" \"House\" entry (file+olp \"~/Documents/org/Task.org\" \"Household\")
         \"* TODO %^{Title}\n\")
        (\"tm\" \"Medical\" entry (file+olp \"~/Documents/org/Task.org\" \"Medical\")
         \"* %^{Status|NOT_BOOKED|BOOKED} %?\nDoctor: %^{Doctor|Mc'G|Lewis|Shell}\nDate: \")

        (\"c\" \"Configs\")
        (\"ce\" \"Emacs\")
        (\"ceo\" \"Org\" entry (file+olp \"~/.dotfiles/Emacs.org\" \"Inbox\" \"Org\")
         \"* TODO %^{Title}\nDescription: %?\")
        (\"cee\" \"Emacs\" entry (file+olp \"~/.dotfiles/Emacs.org\" \"Inbox\" \"General\")
         \"* %^{Title}\n%?\")

        (\"cd\" \"Desktop\")
        (\"cdk\" \"Keybindings\" entry (file+olp \"~/.dotfiles/Desktop.org\" \"Inbox\" \"Keybindings\")
         \"* TODO %^{Function: }\nBinding: =%^{Binding}=\nMap: %^{Keymap: }\")
        (\"cdw\" \"Windows\" entry (file+olp \"~/.dotfiles/Desktop.org\" \"Inbox\" \"Windows\")
         \"* TODO %^{Window}\nDesired Behaviour:%?\")
        (\"cdg\" \"General\" entry (file+olp \"~/.dotfiles/Desktop.org\" \"Inbox\" \"General\")
         \"* TODO %?\")

        (\"cs\" \"System\")
        (\"cso\" \"Os\" entry (file+olp \"~/.dotfiles/System.org\" \"Inbox\" \"Os\")
         \"* TODO %^{Title}\n%?\")
        (\"csm\" \"Manifests\" entry (file+olp \"~/.dotfiles/System.org\" \"Inbox\" \"Manifests\" \"Inbox\")
         \"* %^{Package name: }\nManifest: %^{Manifest: }\")
        (\"csg\" \"General\" entry (file+olp \"~/.dotfiles/System.org\" \"Inbox\" \"General\")
         \"* TODO %^{Title}\")

        (\"cz\" \"Shells\")
        (\"czz\" \"Zsh\" entry (file+olp \"~/.dotfiles/Environment.org\" \"Inbox\" \"ZSH\")
         \"* TODO %^{Title}\")

        (\"i\" \"issues\" entry (file \"~/Documents/org/Issue.org\")
         \"* %^{Issue: }%?\")))\n")
