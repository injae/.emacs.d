# CMake generated Testfile for 
# Source directory: /home/nieel/.emacs.d/elpa/irony-20180703.1740/server/test/elisp
# Build directory: /home/nieel/.emacs.d/elpa/irony-20180703.1740/server/build/test/elisp
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(check-irony-el "/usr/bin/emacs" "-Q" "--batch" "-l" "package" "--eval" "(package-initialize)" "--eval" "(unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/nieel/.emacs.d/elpa/irony-20180703.1740/server/test/elisp/irony.el" "-f" "ert-run-tests-batch-and-exit")
set_tests_properties(check-irony-el PROPERTIES  TIMEOUT "15")
add_test(check-irony-iotask-el "/usr/bin/emacs" "-Q" "--batch" "-l" "package" "--eval" "(package-initialize)" "--eval" "(unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/nieel/.emacs.d/elpa/irony-20180703.1740/server/test/elisp/irony-iotask.el" "-f" "ert-run-tests-batch-and-exit")
set_tests_properties(check-irony-iotask-el PROPERTIES  TIMEOUT "15")
add_test(check-irony-cdb-json-el "/usr/bin/emacs" "-Q" "--batch" "-l" "package" "--eval" "(package-initialize)" "--eval" "(unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/home/nieel/.emacs.d/elpa/irony-20180703.1740/server/test/elisp/irony-cdb-json.el" "-f" "ert-run-tests-batch-and-exit")
set_tests_properties(check-irony-cdb-json-el PROPERTIES  TIMEOUT "15")
