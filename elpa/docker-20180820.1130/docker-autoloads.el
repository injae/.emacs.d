;;; docker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "docker" "docker.el" (23443 23019 600991 809000))
;;; Generated autoloads from docker.el
 (autoload 'docker "docker" nil t)

;;;***

;;;### (autoloads nil "docker-container" "docker-container.el" (23443
;;;;;;  23019 572959 462000))
;;; Generated autoloads from docker-container.el

(autoload 'docker-container-eshell "docker-container" "\
Open `eshell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-container-find-directory "docker-container" "\
Inside CONTAINER open DIRECTORY.

\(fn CONTAINER DIRECTORY)" t nil)

(autoload 'docker-container-find-file "docker-container" "\
Inside CONTAINER open FILE.

\(fn CONTAINER FILE)" t nil)

(autoload 'docker-container-shell "docker-container" "\
Open `shell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-diff "docker-container" "\
Diff the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-inspect "docker-container" "\
Inspect the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-kill "docker-container" "\
Kill the container named NAME using SIGNAL.

\(fn NAME &optional SIGNAL)" t nil)

(autoload 'docker-logs "docker-container" "\
Show the logs from container NAME.

If FOLLOW is set, run in `async-shell-command'.

\(fn NAME &optional FOLLOW)" t nil)

(autoload 'docker-pause "docker-container" "\
Pause the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-rename "docker-container" "\
Rename CONTAINER using NAME.

\(fn CONTAINER NAME)" t nil)

(autoload 'docker-restart "docker-container" "\
Restart the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it.

\(fn NAME &optional TIMEOUT)" t nil)

(autoload 'docker-rm "docker-container" "\
Remove the container named NAME.

With prefix argument, sets FORCE to true.

Force the removal even if the container is running when FORCE is set.
Remove the specified link and not the underlying container when LINK is set.
Remove the volumes associated with the container when VOLUMES is set.

\(fn NAME &optional FORCE LINK VOLUMES)" t nil)

(autoload 'docker-start "docker-container" "\
Start the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-stop "docker-container" "\
Stop the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it.

\(fn NAME &optional TIMEOUT)" t nil)

(autoload 'docker-unpause "docker-container" "\
Unpause the container named NAME.

\(fn NAME)" t nil)

(autoload 'docker-containers "docker-container" "\
List docker containers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-image" "docker-image.el" (23443 23019
;;;;;;  552936 357000))
;;; Generated autoloads from docker-image.el

(autoload 'docker-pull "docker-image" "\
Pull the image named NAME.  If ALL is set, use \"-a\".

\(fn NAME &optional ALL)" t nil)

(autoload 'docker-push "docker-image" "\
Push the image named NAME.

\(fn NAME)" t nil)

(autoload 'docker-rmi "docker-image" "\
Destroy or untag the image named NAME.

Force removal of the image when FORCE is set.
Do not delete untagged parents when NO-PRUNE is set.

\(fn NAME &optional FORCE NO-PRUNE)" t nil)

(autoload 'docker-tag "docker-image" "\
Tag IMAGE using NAME.

\(fn IMAGE NAME)" t nil)

(autoload 'docker-images "docker-image" "\
List docker images.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-machine" "docker-machine.el" (23443
;;;;;;  23019 609001 51000))
;;; Generated autoloads from docker-machine.el

(autoload 'docker-machine-config "docker-machine" "\
Print the connection config for the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-create "docker-machine" "\
Create a machine NAME using DRIVER.

\(fn NAME DRIVER)" t nil)

(autoload 'docker-machine-env "docker-machine" "\
Parse and set environment variables from \"docker-machine env NAME\" output.

\(fn NAME)" t nil)

(autoload 'docker-machine-inspect "docker-machine" "\
Inspect information about the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-ip "docker-machine" "\
Get the IP address of the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-kill "docker-machine" "\
Kill the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-restart "docker-machine" "\
Restart the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-rm "docker-machine" "\
Destroy or uncommand the machine NAME.  If FORCE is set, use \"--force\".

\(fn NAME &optional FORCE)" t nil)

(autoload 'docker-machine-start "docker-machine" "\
Start the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-status "docker-machine" "\
Get the status of the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-stop "docker-machine" "\
Stop the machine NAME.

\(fn NAME)" t nil)

(autoload 'docker-machine-upgrade "docker-machine" "\
Upgrade the machine NAME to the latest version of Docker.

\(fn NAME)" t nil)

(autoload 'docker-machines "docker-machine" "\
List docker machines.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-network" "docker-network.el" (23443
;;;;;;  23019 592982 567000))
;;; Generated autoloads from docker-network.el

(autoload 'docker-network-rm "docker-network" "\
Destroy the network named NAME.

\(fn NAME)" t nil)

(autoload 'docker-networks "docker-network" "\
List docker networks.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-volume" "docker-volume.el" (23443 23019
;;;;;;  580968 704000))
;;; Generated autoloads from docker-volume.el

(autoload 'docker-volume-dired "docker-volume" "\


\(fn NAME)" t nil)

(autoload 'docker-volume-rm "docker-volume" "\
Destroy the volume named NAME.

\(fn NAME)" t nil)

(autoload 'docker-volumes "docker-volume" "\
List docker volumes.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("docker-group.el" "docker-pkg.el" "docker-process.el"
;;;;;;  "docker-utils.el") (23443 23019 625019 535000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; docker-autoloads.el ends here
