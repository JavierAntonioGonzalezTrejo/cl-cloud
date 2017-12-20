(ql:quickload :cl-rabbit)
(ql:quickload :corona)
(ql:quickload :cl-virtualbox)
(ql:quickload :SPLIT-SEQUENCE)
(ql:quickload :uuid)
(ql:quickload :cl-rabbit-tests)
(defpackage :compute
  (:use :common-lisp :cl-rabbit :SPLIT-SEQUENCE :corona :cl-virtualbox :uuid)
  (:export :run-compute))
(in-package :compute)
;;; Javier Antonio Gonzalez Trejo 29100317
(defparameter *compute-user* "compute")
(defparameter *compute-pass* "compute")
(defparameter *virtual-host* "cloud")
(defparameter *hostname* "debianProblem")
(defparameter *ssl-port* 5671)
(defparameter *cacert-path* "./testca")
(defparameter *cacert-name* "/cacert.pem")
(defparameter *computecert-path* "./client")
(defparameter *computecert-cert* "cert.pem")
(defparameter *computecert-key* "key.pem")
(defparameter *exchange-petition* "petition")

(defparameter *make-workloads-command* "create-workload")
(defparameter *list-workloads-command* "list-workload")
(defparameter *make-instances-command* "create-instance")
(defparameter *start-instance-command* "start-instance")
(defparameter *stop-instance-command* "stop-instance")
(defparameter *list-instance-command* "list-instance")
(defparameter *bind-ip-instance-command* "bind-ip-instance")
(defparameter *bind-ip-instance-command-network* "bind-ip-instance-network")

(defparameter *workload-list* (list))
(defparameter *instance-list* (list))
(defparameter *working-instances-list* (list))

(defun receive-result-bind-request (correlation-id queue-name)
  (with-connection (conexion)
    (let ((socket (ssl-socket-new conexion)))
      (ssl-socket-set-cacert socket (concatenate 'string *cacert-path* "/" *cacert-name*))
      (ssl-socket-set-key socket (concatenate 'string *computecert-path* "/" *computecert-cert*)
                          (concatenate 'string *computecert-path* "/" *computecert-key*))
      (socket-open socket *hostname* *ssl-port*)
      (login-sasl-plain conexion *virtual-host* *compute-user* *compute-pass*)
      (channel-open conexion 1)
      (queue-declare conexion 1  :queue queue-name) 
      (loop while t
	 do 
	   (basic-consume conexion 1 queue-name)
           (let* ((result (consume-message conexion))
                  (message (envelope/message result))
                  (mess-props (message/properties message)) 
                  (body-message (babel:octets-to-string (message/body message):encoding :utf-8)) 
                  (message-id (cdr (second mess-props))))
             (cond ((string= correlation-id message-id)
                    (basic-ack conexion 1 (envelope/delivery-tag result))
                    (return-from receive-result-bind-request body-message))))))))

(defun bind-ip-instance (parameters-string)
  (let* ((parameters-list (split-sequence #\space parameters-string))
         (name-instance (string-upcase (first parameters-list)))
         (parameters-command-bind-network (concatenate 'string *bind-ip-instance-command-network* " " parameters-string))
         (compute-queue "compute") 
         (correlation-id (print-bytes nil (make-v4-uuid)))) 
    (if (not (find-workload name-instance *instance-list*))  
        (return-from bind-ip-instance "The instance does not exist")) 
    (send-compute-petition parameters-command-bind-network compute-queue correlation-id)
    (receive-result-bind-request correlation-id compute-queue)))

(defun send-compute-petition (message reply-to-compute correlation-id)
  (with-connection (conexion)
    (let ((socket (ssl-socket-new conexion)))
      (ssl-socket-set-cacert socket (concatenate 'string *cacert-path* "/" *cacert-name*))
      (ssl-socket-set-key socket (concatenate 'string *computecert-path* "/" *computecert-cert*)
                          (concatenate 'string *computecert-path* "/" *computecert-key*))
      (socket-open socket *hostname* *ssl-port*)
      (login-sasl-plain conexion *virtual-host* *compute-user* *compute-pass*)
      (channel-open conexion 1)
      (exchange-declare conexion 1 *exchange-petition* "fanout")
      (basic-publish conexion 1
                     :exchange *exchange-petition*
                     :routing-key ""
                     :body message
                     :properties `((:reply-to . ,reply-to-compute)
                                   (:delivery-mode . 2) 
                                   (:correlation-id . ,correlation-id))))))

(defun concat-with-space( list )
  (format nil "~{~a~^ ~}" list))

(defun create-workload-corona (message)
  (let* ((list-parameters-workload (split-sequence #\Space message))
         (name-workload (first list-parameters-workload))
         (os-name (second list-parameters-workload))
         (os-version (third list-parameters-workload))
         (os-arch (nth 3 list-parameters-workload))
         (memory (nth 4 list-parameters-workload))
         (cpu-count (nth 5 list-parameters-workload))
         (name-workload-symbol (intern (string-upcase name-workload)))
         (upcased-message (string-upcase message)))
    (if (not (find-workload (string-upcase name-workload) *workload-list*))
        (setf *workload-list* (nconc *workload-list* (list upcased-message)))
        (return-from create-workload-corona "The Workload already exists")) 
    (eval `(corona:defmachine ,name-workload-symbol
               :system (,(intern (string-upcase os-name) "KEYWORD")
                         ,(intern (string-upcase os-version) "KEYWORD")
                         ,(intern (string-upcase os-arch) "KEYWORD"))
               :memory ,(parse-integer memory)
               :cpu-count ,(parse-integer cpu-count)))    
    name-workload))


(defun list-workload (workloads-list)
  (cond ((not workloads-list) (return-from list-workload "")))
  (let* ((workload (pop workloads-list))
         (parameters-list (split-sequence #\Space workload)))
    (concatenate 'string
                 (format nil "---Workload: ~s. OS: ~s. OS version: ~s. OS Architecture: ~s. Memory: ~s. Number of CPUs: ~s~C" 
                         (first parameters-list)
                         (second parameters-list)
                         (third parameters-list)
                         (nth 3 parameters-list)
                         (nth 4 parameters-list)
                         (nth 5 parameters-list)
                         #\newline)
                 (list-workload workloads-list))))

(defun list-working-instances (working-instance-list)
  (cond ((not working-instance-list)
         (return-from list-working-instances "")))
  (let ((instance (pop working-instance-list)))
    (concatenate 'string (format nil "---Instance Name: ~s~C"
                                 instance
                                 #\Newline)
                 (list-working-instances working-instance-list))))

(defun list-instances (instances-list)
  (cond ((not instances-list)
         (return-from list-instances (format nil "###############~C" #\newline))))
  (let* ((instance (pop instances-list))
         (instance-string-list (split-sequence #\space instance)))
    (concatenate 'string
                 (format nil "---Instance name: ~s. Linked Workload: ~s.~C"
                         (first instance-string-list)
                         (second instance-string-list)
                         #\newline)
                 (list-instances instances-list))))

(defun list-all-instances (instances-list working-instance-list)
  (concatenate 'string (list-instances instances-list) (list-working-instances working-instance-list)))

(defun find-workload (name-new-workload workloads-list)
  "Returns nil if not workload is found. t otherwise."
  (cond ((not workloads-list) (return-from find-workload nil)))
  (let* ((workload (pop workloads-list))
         (parameters-list (split-sequence #\space workload))
         (name-workload (first parameters-list))) 
    (or (string= name-new-workload name-workload)
        (find-workload name-new-workload workloads-list))))

(defun send-result-operation (message routing-key correlation-id)
  (with-connection (conexion)
    (let ((socket (ssl-socket-new conexion)))
      (ssl-socket-set-cacert socket (concatenate 'string *cacert-path* "/" *cacert-name*))
      (ssl-socket-set-key socket (concatenate 'string *computecert-path* "/" *computecert-cert*)
                          (concatenate 'string *computecert-path* "/" *computecert-key*))
      (socket-open socket *hostname* *ssl-port*)
      (login-sasl-plain conexion *virtual-host* *compute-user* *compute-pass*)
      (channel-open conexion 1)
      (queue-declare conexion 1
                     :queue routing-key)
      (basic-publish conexion 1
                     :exchange ""
                     :routing-key routing-key
                     :body message
                     :properties `((:delivery-mode . 2)
                                   (:correlation-id . ,correlation-id))))))

(defun create-instance (message)
  (let* ((list-parameters-instance (split-sequence #\space message))
         (name-instance (string-upcase (first list-parameters-instance)))
         (name-workload (string-upcase (second list-parameters-instance)))
         (upcased-message (string-upcase message)))
    (if (not (find-workload name-workload *workload-list*))
        (return-from create-instance "The workload does not exist")
        (if (not (find-workload name-instance *instance-list*))
            (setf *instance-list* (nconc *instance-list* (list upcased-message)))
            (return-from create-instance "The instance already exists")))
    (first list-parameters-instance)))

(defun check-item-instance-list (name-instance instance-to-check)
  "Checks the if the name-instance is on an item of instance-list. This is used becaused an element of the instance-list is composed by a string \"name-instance name-workload\" on a single string separated by spaces."
  (let* ((instance-to-check-on-list (split-sequence #\space instance-to-check))
         (instance-to-check-name (first instance-to-check-on-list))
         (upcased-name-instance (string-upcase name-instance)))
    (if (string= upcased-name-instance instance-to-check-name)
        (return-from check-item-instance-list t)
        (return-from check-item-instance-list nil))))

(defun start-instance (name-instance)
  (let ((instance-index (position name-instance *instance-list* :test #'check-item-instance-list)))
    (if (null instance-index)
        (return-from start-instance "Instance do not exist"))
    (let* ((instance-string (nth instance-index *instance-list*))
           (instance-item-string-list (split-sequence #\Space instance-string))
           (instance-workload (second instance-item-string-list))
           (upcased-name-instance (string-upcase name-instance)))
      (if (not (null (position upcased-name-instance *working-instances-list* :test #'string=)))
          (return-from start-instance "Instance already running."))
      (eval `(start ,(intern instance-workload)))
      (setf *working-instances-list* (nconc *working-instances-list* (list upcased-name-instance)))
      "Instance ready to work!!!")))

(defun stop-instance (name-instance)
  (let ((instance-index (position name-instance *instance-list* :test #'check-item-instance-list)))
    (if (null instance-index)
        (return-from stop-instance "Instance does not exist"))
    (let* ((instance-string (nth instance-index *instance-list*))
           (instance-item-string-list (split-sequence #\Space instance-string))
           (instance-workload (second instance-item-string-list))
           (upcased-name-instance (string-upcase name-instance)))
      (if (null (position upcased-name-instance *working-instances-list* :test #'string=))
          (return-from stop-instance "Instance not running."))
      (eval `(stop ,(intern instance-workload)))
      (setf *working-instances-list* (remove upcased-name-instance *working-instances-list* :test #'string=))
      "Instance stopped!!!")))

(defun run-compute() 
  "Creates workloads, instances based on the workloads. Send petitions to use the network and obey binding ip to the intances."  
  (with-connection (conexion)
    (let ((socket (ssl-socket-new conexion))
	  (queue-name (cl-rabbit.tests::make-random-name)))
      (ssl-socket-set-cacert socket (concatenate 'string *cacert-path* "/" *cacert-name*))
      (ssl-socket-set-key socket (concatenate 'string *computecert-path* "/" *computecert-cert*)
                          (concatenate 'string *computecert-path* "/" *computecert-key*))
      (socket-open socket *hostname* *ssl-port*)
      (login-sasl-plain conexion *virtual-host* *compute-user* *compute-pass*)
      (channel-open conexion 1)
      (exchange-declare conexion 1 *exchange-petition* "fanout")
      (queue-declare conexion 1  :queue queue-name
                     :exclusive t)
      (queue-bind conexion 1
                  :queue queue-name
                  :exchange *exchange-petition*)
      (loop while t
         do 
           (basic-consume conexion 1 queue-name)
           (let* ((result (consume-message conexion))
                  (message (envelope/message result))
                  (mess-props (message/properties message)) 
                  (body-message (babel:octets-to-string (message/body message):encoding :utf-8))
                  (list-body-message (split-sequence #\space body-message))
                  (command-received (pop list-body-message))
                  (parameters-command (concat-with-space list-body-message))
                  (message-id (cdr (second mess-props)))
                  (queue-routing-key (cdr (third mess-props))))
             (cond
               ((string= *make-workloads-command* command-received)
                (let ((result (create-workload-corona parameters-command)))
                  (send-result-operation                   
                   result
                   queue-routing-key
                   message-id)))
               
               ((string= *list-workloads-command* command-received)
                (send-result-operation                 
                 (list-workload *workload-list*)
                 queue-routing-key
                 message-id))
               
               ((string= *make-instances-command* command-received)
                (let ((result (create-instance parameters-command)))
                  (send-result-operation                   
                   result
                   queue-routing-key
                   message-id)))
               ((string= *start-instance-command* command-received)
                (let ((result (start-instance parameters-command)))
                  (send-result-operation                   
                   result
                   queue-routing-key
                   message-id)))
               ((string= *stop-instance-command* command-received)
                (let ((result (stop-instance parameters-command)))
                  (send-result-operation                   
                   result
                   queue-routing-key
                   message-id)))
               ((string= *list-instance-command* command-received)
                (let ((result (list-all-instances *instance-list* *working-instances-list*)))
                  (send-result-operation                   
                   result
                   queue-routing-key
                   message-id)))
               ((string= *bind-ip-instance-command* command-received)
                (let ((result (bind-ip-instance parameters-command)))
                  (send-result-operation                   
                   result
                   queue-routing-key
                   message-id))))
             (basic-ack conexion 1 (envelope/delivery-tag result)))))))

