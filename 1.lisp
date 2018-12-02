(defun part1 (file)
  (with-open-file (s file)
    (loop :for x = 0 :then (read s nil) :while x :sum x)))

(defun part2 (file)
  (with-open-file (s file)
    (loop
       :with freq = 0 :and history = nil
       :for diff = (read s nil)
       :when (null diff) :do (progn (file-position s 0)
				    (setq diff (read s nil)))
       :do (incf freq diff)
       :when (member freq history) :return freq
       :do (pushnew freq history))))
