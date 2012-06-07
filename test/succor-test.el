(require 'ert)
(require  'mocker)

(ert-deftest succor-test:get-current-project ()
  (mocker-let ((gtags-get-rootpath ()
								   ((:output "C:/cygwin/home/user/project/"))))
	(should (equal (succor-get-current-project) "project")))
  (mocker-let ((gtags-get-rootpath ()
								   ((:output "/home/user/project/"))))
	(should (equal (succor-get-current-project) "project")))
  (mocker-let ((gtags-get-rootpath ()
								   ((:output "~/project/"))))
	(should (equal (succor-get-current-project) "project")))



;					 									  (:input '(linux) :output "/home/project/"))))
