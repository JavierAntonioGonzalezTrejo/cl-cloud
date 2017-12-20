(ql:quickload :cl-rabbit)
(ql:quickload :uuid)
(defpackage :controller-logic
  (:use :common-lisp :cl-rabbit :uuid)
  (:export :controller-execute
           :concat-with-space))
;;; Javier Antonio Gonzalez Trejo 29100317
(in-package :controller-logic)
(defparameter *controller-user* "controller")
(defparameter *controller-pass* "controller")
(defparameter *virtual-host* "cloud")
(defparameter *hostname* "debianProblem")
(defparameter *ssl-port* 5671)
(defparameter *cacert-path* "./testca")
(defparameter *cacert-name* "/cacert.pem")
(defparameter *controllercert-path* "./client")
(defparameter *controllercert-cert* "cert.pem")
(defparameter *controllercert-key* "key.pem")
(defparameter *queue-receive* "controller")
(defparameter *exchange-petition* "petition")

(defun receive-result (action-took)
  (with-connection (conexion)
    (let ((socket (ssl-socket-new conexion)))
      (ssl-socket-set-cacert socket (concatenate 'string *cacert-path* "/" *cacert-name*))
      (ssl-socket-set-key socket (concatenate 'string *controllercert-path* "/" *controllercert-cert*)
                          (concatenate 'string *controllercert-path* "/" *controllercert-key*))
      (socket-open socket "debianProblem" 5671)
      (login-sasl-plain conexion *virtual-host* *controller-user* *controller-pass*)
      (channel-open conexion 1)
      
      (queue-declare conexion 1
                     :queue *queue-receive*)
      (loop while t
         do
           (basic-consume conexion 1 *queue-receive*)
           (let* ((result (consume-message conexion))
                  (message (envelope/message result))
                  (mess-props (message/properties message))
                  (command-recived (cdr (second mess-props))))
             (cond
               ((string= action-took command-recived)
                (basic-ack conexion 1 (envelope/delivery-tag result))
                (return-from receive-result (babel:octets-to-string (message/body message):encoding :utf-8)))))))))

(defun concat-with-space( list )
  (format nil "~{~a~^ ~}" list))


(defun controller-execute (parameters-string cloud-operation)
  "Send a message to the controller or network to create a workload, instances or bind ip's.
parameters-list a string separated by spaces containing the following data:
ALL MESSAGES START WHIT THE *cloud-operation*.
1.)For Crete Workloads:
*cloud-operation*: \"create-workload\"
*parameters-string*: \"workload name os osversion mem cpu\"
name: Name of the workload to create.
os: Name of the OS to use.
osversion: Version of the OS to use.
osarch: Architecture of the OS to use.
mem: how much ram the workload will have on MB.
cpu: How much cores of the compute node the workload whill utilice.
Returns the name of the workload on secceed otherwise returns the \"Workload Already Exists\".
##################################################################
2.)To list Workloads
*cloud-operation*: \"list-workload\"
*parameters-string*: \"\"
Returns a string listing all the available workloads on the following format Workload: ~s. OS: ~s. OS version: ~s. OS Architectur: ~s. Memory: ~s. Number of CPUs: ~s~C from the variable *workload-list*
##################################################################
3.)To create instances
*cloud-operation*:\"create-instance\"
*parameters-string*:\"name-instance name-workload\"
name-instance: Name to of the instance to create.
name-workload: Name of the workload to create.
Creates an instance based on the all ready created on the workload. returns the name of the instance if succeds. Otherwise returns \"Instance already exists\" or \"Workload does not exit\" depending. Currently only one instnce can be created by workload.
##################################################################
4.)To list instances
*cloud-operation: \"list-instance\"
*parameters-string*:\"\"
Returns a string listing all the available and running instances."
  (with-connection (conexion)
    (let ((socket (ssl-socket-new conexion)) 
          (parameters-plus-command-string (concatenate 'string cloud-operation " " parameters-string ))
          (message-id (print-bytes nil (make-v4-uuid))))
      (ssl-socket-set-cacert socket (concatenate 'string *cacert-path* "/" *cacert-name*))
      (ssl-socket-set-key socket (concatenate 'string *controllercert-path* "/" *controllercert-cert*)
                          (concatenate 'string *controllercert-path* "/" *controllercert-key*))
      (socket-open socket "debianProblem" 5671)
      (login-sasl-plain conexion *virtual-host* *controller-user* *controller-pass*)
      (channel-open conexion 1)
      (exchange-declare conexion 1 *exchange-petition* "fanout") 
      (basic-publish conexion 1
                     :exchange *exchange-petition*
                     :routing-key ""
                     :body parameters-plus-command-string
                     :properties `((:reply-to . ,*queue-receive*)
                                   (:delivery-mode . 2)
                                   (:correlation-id . ,message-id)))
      (receive-result message-id))))
