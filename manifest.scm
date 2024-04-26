(use-modules (guix packages)
             (gnu packages base)
             (gnu packages compression)
             (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest (list guile-next guile-hoot gnu-make zip))
