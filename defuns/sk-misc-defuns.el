;; Debug configuration for ROS Turtlebots using LSP DAP mode
(defun sk/turtlebot-debugging-configurations ()
  "ROS debug template with violet as the master and its odometry values"
  (interactive)
  (dap-register-debug-template
   "Python :: Turtlebot Subscribe Violet Master Violet Odom"
   (list :type "python"
		 :args "violet_measure.json"
		 :cwd  (concat (getenv "HOME")
					   "/Dropbox/Lab/Turtlebot/multi_agent/scripts")
		 :target-module nil
		 :request "launch"
		 :name "Python :: Turtlebot Subscribe Violet Master Violet Odom")))

;; Turtlebot masters
(defun sk/turtlebot-violet-master ()
  "Set violet as the ROS master for the Turtlebot"
  (interactive)
  (setenv "ROS_MASTER_URI" "http://10.0.1.5:11311"))
(defun sk/turtlebot-blue-master ()
  "Set blue as the ROS master for the Turtlebot"
  (interactive)
  (setenv "ROS_MASTER_URI" "http://10.0.1.8:11311"))
(defun sk/turtlebot-green-master ()
  "Set green as the ROS master for the Turtlebot"
  (interactive)
  (setenv "ROS_MASTER_URI" "http://10.0.1.16:11311"))
(defun sk/turtlebot-yellow-master ()
  "Set yellow as the ROS master for the Turtlebot"
  (interactive)
  (setenv "ROS_MASTER_URI" "http://10.0.1.13:11311"))
(defun sk/turtlebot-orange-master ()
  "Set orange as the ROS master for the Turtlebot"
  (interactive)
  (setenv "ROS_MASTER_URI" "http://10.0.1.11:11311"))
(defun sk/turtlebot-red-master ()
  "Set red as the ROS master for the Turtlebot"
  (interactive)
  (setenv "ROS_MASTER_URI" "http://10.0.1.15:11311"))

;; provide this configuration
(provide 'sk-misc-defuns)
