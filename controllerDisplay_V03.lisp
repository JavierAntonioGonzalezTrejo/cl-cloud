(ql:quickload :ltk)
(load "~/Documents/computoParaleloDistribuido/proyectoRabbit/controller_V03.lisp")
(defpackage :controller
  (:use :common-lisp :ltk :controller-logic)
  (:export :main
           :externalIPDisplay
           :workloadDisplay
           ))
;;; Javier Antonio Gonzalez Trejo 29100317
(in-package :controller)
(defparameter *make-workloads-command* "create-workload")
(defparameter *list-workloads-command* "list-workload")
(defparameter *make-instances-command* "create-instance")
(defparameter *start-instance-command* "start-instance")
(defparameter *stop-instance-command* "stop-instance")
(defparameter *list-instance-command* "list-instance")
(defparameter *bind-ip-instance-command* "bind-ip-instance")
(defparameter *make-net-command* "make-net")
(defparameter *list-net-command* "list-net")
(defparameter *unbind-ip-instance-command* "unbind-ip-instance")
(defparameter *list-ip-instance-command* "list-ip-instance")

(defun trim-non-visible-characters (string-to-trim)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) string-to-trim))

(defun main ()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
           (externalIP (make-instance 'button
                                      :master f
                                      :text "External IPs"
                                      :command (lambda ()
                                                 (externalIPDisplay))))
           (vm (make-instance 'button
                              :master f
                              :text "Manage Instances"
                              :command (lambda ()
                                         (instances-display))))
           (storage (make-instance 'button
                                   :master f
                                   :text "Manage Workloads"
                                   :command (lambda ()
                                              (workloaddisplay)))))
      (pack f)
      (pack externalIP)
      (pack vm)
      (pack storage)
      )))


(defun externalIPDisplay ()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
           (mssNet (make-instance 'message
                                  :master f
                                  :text "Create a new net"
                                  :aspect 300))

           (txtSubNet (make-instance 'text
                                     :master f
                                     :width 15
                                     :height 1))
           ( mssMask (make-instance 'message
                                    :master f
                                    :text "Subnet bits"
                                    :aspect 300))
           (txtMask (make-instance 'text
                                   :master f
                                   :width 2
                                   :height 1))
           (mssNameNet (make-instance 'message
                                      :master f
                                      :text "Name of the Subnet"
                                      :aspect 300))
           (txtNameNet (make-instance 'text
                                      :master f
                                      :width 50
                                      :height 1))
           (btnMakeNet (make-instance 'button
                                      :master f
                                      :text "Make net"))
           (btnlistnet (make-instance 'button
                                      :master f
                                      :text "Lists nets"))

           (mssBindIP (make-instance 'message
                                     :master f
                                     :text "Bind an IP to a Instance"
                                     :aspect 400))
           (txtInstance (make-instance 'text
                                       :master f
                                       :width 50
                                       :height 1))
           ( mssNameNetBind (make-instance 'message
                                           :master f
                                           :text "Name of the Subnet"
                                           :aspect 300))
           (txtNameNetBind (make-instance 'text
                                          :master f
                                          :width 50
                                          :height 1))
           (btnBindIP (make-instance 'button
                                     :master f
                                     :text "Bind IP"))
           (mssUnBind (make-instance 'message
                                     :master f
                                     :text "UnBind IP of an Instance"
                                     :aspect 300))
           (txtInstanceUnbind (make-instance 'text
                                             :master f
                                             :width 50
                                             :height 1))
           (btnUnBindIP (make-instance 'button
                                       :master f
                                       :text "Unbind IP"))
           (btnListBindIPs (make-instance 'button
                                          :master f
                                          :text "List Instances IPs")))

      (bind btnMakeNet "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-net (trim-non-visible-characters (text txtNameNet)))
                     (ip-net (trim-non-visible-characters (text txtSubNet)))
                     (cidr (trim-non-visible-characters (text txtMask)))
                     (parameters-string (concat-with-space (list name-net ip-net cidr)))
                     (result (controller-execute parameters-string *make-net-command*)))
                (do-msg result :title "Result")
                )))
      (bind btnlistnet "<ButtonPress-1>"
            (lambda (evt)
              (let ((result (controller-execute "" *list-net-command*)))
                (do-msg result :title "List"))))

      (bind btnBindIP "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-instance (trim-non-visible-characters (text txtInstance)))
                     (name-net (trim-non-visible-characters (text txtNameNetBind)))
                     (parameters-string (concat-with-space (list name-instance name-net)))
                     (result (controller-execute parameters-string *bind-ip-instance-command*)))
                (do-msg result :title "Result"))))
      
      (bind btnUnBindIP "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-instance (trim-non-visible-characters (text txtInstanceUnbind))) 
                     (result (controller-execute name-instance *unbind-ip-instance-command*)))
                (do-msg result :title "Result"))))

      (bind btnListBindIPs "<ButtonPress-1>"
            (lambda (evt)
              (let ((result (controller-execute "" *list-ip-instance-command*)))
                (do-msg result :title "List"))))
      
      (grid f 0 0)
      (grid mssNet 0 0)
      (grid txtSubNet 1 0)
      (grid mssMask 2 0)
      (grid txtMask 3 0)
      (grid mssNameNet 4 0)
      (grid txtNameNet 5 0)
      (grid btnMakeNet 6 0)
      (grid btnlistnet 7 0)
      (grid mssBindIP 0 1)
      (grid txtInstance 1 1)
      (grid mssNameNetBind 2 1)
      (grid txtNameNetBind 3 1)
      (grid btnBindIP 4 1)
      (grid btnListBindIPs 5 1)
      (grid mssUnBind 0 2)
      (grid txtInstanceUnbind 1 2)
      (grid btnUnBindIP 2 2) 
      )))


(defun workloaddisplay()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
           (mssWorkload (make-instance 'message
                                       :master f
                                       :text "Create Workload"
                                       :aspect 300))

           (txtWorkload (make-instance 'text
                                       :master f
                                       :width 50
                                       :height 1))
           (mssOS (make-instance 'message
                                 :master f
                                 :text "Operation system"
                                 :aspect 300))
           (txtOS (make-instance 'text
                                 :master f
                                 :width 50
                                 :height 1))
           (mssVersionOS (make-instance 'message
                                        :master f
                                        :text "Version"
                                        :aspect 300))
           (txtVersion (make-instance 'text
                                      :master f
                                      :width 10
                                      :height 1))

           (mssArchOS (make-instance 'message
                                     :master f
                                     :text "Architecture"
                                     :aspect 400))
           (txtArch (make-instance 'text
                                   :master f
                                   :width 2
                                   :height 1))

           (mssMemory (make-instance 'message
                                     :master f
                                     :text "Ram memory (MB)"
                                     :aspect 300))
           (txtMemory (make-instance 'text
                                     :master f
                                     :width 5
                                     :height 1))
           (mssCPU (make-instance 'message
                                  :master f
                                  :text "Number CPUs"
                                  :aspect 300))
           (txtCPU (make-instance 'text
                                  :master f
                                  :width 3
                                  :height 1))

           (btnMakeWorkload (make-instance 'button
                                           :master f
                                           :text "Make workload"))
           (btnListWorkLoad (make-instance 'button
                                           :master f
                                           :text "List Workloads")))

      ;; Create workload
      (bind btnMakeWorkload "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-workload (trim-non-visible-characters (text txtWorkload)))
                     (os-name (trim-non-visible-characters (text txtOS)))
                     (os-version (trim-non-visible-characters (text txtVersion)))
                     (os-arch (trim-non-visible-characters (text txtArch)))
                     (memory (trim-non-visible-characters (text txtMemory)))
                     (cpu-count (trim-non-visible-characters (text txtCPU)))
                     (parameters-list (list name-workload os-name os-version os-arch memory cpu-count))
                     (parameters-string (concat-with-space parameters-list)))
                (let ((result-workload-operation (controller-execute parameters-string *make-workloads-command*)))
                  (do-msg result-workload-operation :title "Result")))))
      ;; List workloads
      (bind btnListWorkLoad "<ButtonPress-1>"
            (lambda (evn)
              (let ((result-list-workload (controller-execute "" *list-workloads-command*)))
                (do-msg result-list-workload :title "List"))))
      (grid f 0 0)
      (grid mssWorkload 0 0)
      (grid txtWorkload 1 0)
      (grid mssOS 2 0)
      (grid txtOS 3 0)
      (grid mssVersionOS 4 0)
      (grid txtVersion 5 0)
      (grid mssArchOS 6 0)
      (grid txtArch 7 0)
      (grid mssMemory 8 0)
      (grid txtMemory 9 0)
      (grid mssCPU 10 0)
      (grid txtCPU 11 0)
      (grid btnMakeWorkload 12 0)

      (grid btnListWorkLoad 0 1)

      )))

(defun instances-display()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
           (mssinstance (make-instance 'message
                                       :master f
                                       :text "Create a new Instance"
                                       :aspect 300))

           (txtnameinstance (make-instance 'text
                                           :master f
                                           :width 50
                                           :height 1))
           ( mssworkload (make-instance 'message
                                        :master f
                                        :text "Name Workload"
                                        :aspect 300))
           (txtworkload (make-instance 'text
                                       :master f
                                       :width 50
                                       :height 1))
           (btnMakeinstance (make-instance 'button
                                           :master f
                                           :text "Make Instance"))
           (mssstart (make-instance 'message
                                    :master f
                                    :text "Start Instance"
                                    :aspect 400))
           (txtInstancestart (make-instance 'text
                                            :master f
                                            :width 50
                                            :height 1))
           (btnstartinstance (make-instance 'button
                                            :master f
                                            :text "Start Instance"))
           (mssstopinstance (make-instance 'message
                                           :master f
                                           :text "Stop Instance"
                                           :aspect 300))
           (txtinstancestop (make-instance 'text
                                           :master f
                                           :width 50
                                           :height 1))
           (btnstopinstance (make-instance 'button
                                           :master f
                                           :text "Stop Instance"))
           (btnlistinstances (make-instance 'button
                                            :master f
                                            :text "List Instances")))

      (bind btnMakeinstance "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-instance (trim-non-visible-characters (text txtnameinstance)))
                     (name-workload (trim-non-visible-characters (text txtworkload)))
                     (parameters-list (list name-instance name-workload))
                     (parameters-string (concat-with-space parameters-list))
                     (result-make-instance (controller-execute parameters-string *make-instances-command*)))
                (do-msg result-make-instance :title "Result"))))

      (bind btnstartinstance "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-instance (trim-non-visible-characters (text txtInstancestart)))
                     (result-start-instance (controller-execute name-instance *start-instance-command*)))
                (do-msg result-start-instance :title "Result"))))

      (bind btnstopinstance "<ButtonPress-1>"
            (lambda (evt)
              (let* ((name-instance (trim-non-visible-characters (text txtinstancestop)))
                     (result-stop-instance (controller-execute name-instance *stop-instance-command*)))
                (do-msg result-stop-instance :title "Result"))))
      (bind btnlistinstances "<ButtonPress-1>"
            (lambda (evt)
              (let ((result-list-instances (controller-execute "" *list-instance-command*)))
                (do-msg result-list-instances :title "List"))))

      (grid f 0 0)
      (grid mssinstance 0 0)
      (grid txtnameinstance 1 0)
      (grid mssworkload 2 0)
      (grid txtworkload 3 0)
      (grid btnMakeinstance 4 0)
      (grid mssstart 0 1)
      (grid txtInstancestart 1 1)
      (grid btnstartinstance 2 1)
      (grid mssstopinstance 0 2)
      (grid txtinstancestop 1 2)
      (grid btnstopinstance 2 2)
      (grid btnlistinstances 0 3))))
