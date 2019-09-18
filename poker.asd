(asdf:defsystem :poker
    :version "0.1"
    :description "No Limit Texas Holdem"
    :author "Andy Driver <andy@pagezero.net>"
    :license "MIT"
    :serial t
    :components ((:file "package")
                 (:file "utils")
                 (:file "cards")
                 (:file "game")
                 (:file "table")
                 (:file "player")
                 (:file "hand")
                 (:file "poker")
                 (:file "ui")
                 (:file "main")))
