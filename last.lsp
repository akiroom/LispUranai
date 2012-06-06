
;�ե��ܥʥå������׻�����ؿ��������˻��֤�������Τǡ�wait������뤿��˻Ȥ�
(defun fibo (x)
  (if (or (= 0 x) (= 1 x))
      1
      (+ (fibo (- x 1)) (fibo (- x 2)))))

;���̤�õ��ؿ�
(defun screen-clear(x)
  (progn
  (dotimes (i x)
    (print '��)
  )
  (fibo 20)
  (print '��)
  )
)

;���˥᡼����󤵤���ؿ���data�ꥹ�Ȥ���Ȥ�x��ɽ������
(defun anime(x data)
  (dotimes (i x)
    (progn
      (princ (nth (mod i (length data)) data) )
      (screen-clear 19)
    )
  )
)

;�ե����뤫���ɤ߹���ǥꥹ�Ȥˤ���ؿ�
(defun read-file-list (file)
  (with-open-file (fp file :direction :input)
    (let ((line nil) (kekka nil))
      (loop
        (setf line (read-line fp nil))
        (if line (if (string= "" line) (return (reverse kekka)) (push line kekka)) (return (reverse kekka)))
      )
    )
  )
)

;�ե����뤫���ɤ߹�����ǡ����򥢥˥Ჽ����
(defun file-anime (file num)
  (anime num (read-file-list file))
)
;�ޥ���饤���б��Υե����뤫���ɤ߹�����ǡ����򥢥˥Ჽ����
(defun file-anime-m (file num)
  (anime-m num (read-file-list-m file "��"))
)

;�����ǡ����١������ɤ߹���
(load "kanji.lsp")

;�ꤤ����
(defun uranai ()
  (progn
    (title)
    (2linemes "This program could be running on EUC-JP")
    (commandmes "̾�������Ϥ��Ƥ��������� (ex. ����)")
    (setq fam (string (read)))
    (commandmes "̾�������Ϥ��Ƥ��������� (ex. ��Ϻ)")
    (setq las (string (read)))
    (file-anime "calculating.anime" 77)
    (2linemes (format nil "~A ~A����" fam las))
    (returnn 2)
    (2linemes (format nil "���ʤ�����ʤ�~D�衧~A" (soukaku fam las) (get-num-lucky (soukaku fam las))))
    (2linemes (format nil "���ʤ���ŷ�ʤ�~D�衧~A" (tenkaku fam) (get-num-lucky (tenkaku fam))))
    (2linemes (format nil "���ʤ��οͳʤ�~D�衧~A" (jinkaku fam las) (get-num-lucky (jinkaku fam las))))
    (2linemes (format nil "���ʤ����ϳʤ�~D�衧~A" (chikaku las) (get-num-lucky (chikaku las))))
    (2linemes (format nil "���ʤ��γ��ʤ�~D�衧~A" (gaikaku fam las) (get-num-lucky (gaikaku fam las))))
    (returnn 2)
    (princ (soukaku-comment (convert-str2sym-with-num (get-num-lucky (soukaku fam las)) 3) ) )
    (princ (tenkaku-comment (convert-str2sym-with-num (get-num-lucky (tenkaku fam)) 3) ) )
    (princ (jinkaku-comment (convert-str2sym-with-num (get-num-lucky (jinkaku fam las)) 3) ) )
    (princ (chikaku-comment (convert-str2sym-with-num (get-num-lucky (chikaku las)) 3) ) )
    (princ (gaikaku-comment (convert-str2sym-with-num (get-num-lucky (gaikaku fam las)) 3) ) )
    (returnn 4)
  )
)

;�����դ���princ
(defun 2linemes (mes)
  (progn
  (princ mes)
  (terpri)
  )
)
;>�ä��դ��Ƥ�2linemes
(defun commandmes (mes)
  (progn
  (2linemes mes)
  (princ ">")
  )
)
;���ꤷ��������Ԥ���ؿ�
(defun returnn (num)
  (progn
  (dotimes (i (1- num))
    (terpri)
  )
    (princ '��)
  )
)

(defun title()
  (progn
    (princ "�������� ��_,,��")
    (terpri)
    (princ ".��/��. (�����؎���) ������")
    (terpri)
    (princ "��| �� ����������  �� |���������������徺����ʤ�������")
    (terpri)
    (princ "��������/ ���������� /")
    (terpri)
    (princ "���������ʥ΢ޡ���")
    (terpri)
  )
)

;���
(defun soukaku (fam las)
  (+ (tenkaku fam) (chikaku las))
)
;ŷ��
(defun tenkaku (fam &optional (pos 0) (sum 0))
  (if (= pos (length fam)) sum
    (tenkaku fam (1+ pos) (+ sum (get-count-str-kanji fam pos) ) )
))
;�ͳ�
(defun jinkaku (fam las)
  (+ (get-count-str-kanji fam (1- (length fam))) (get-count-str-kanji las 0) )
)
;�ϳ�
(defun chikaku (las &optional (pos 0) (sum 0))
  (if (= pos (length las)) sum
    (chikaku las (1+ pos) (+ sum (get-count-str-kanji las pos) ) )
))
;����
(defun gaikaku (fam las)
  (+ (get-count-str-kanji fam 0) (get-count-str-kanji las (1- (length las))) )
)




;��ʥ�����
(defun soukaku-comment (mark)
  (let ((comment (pairlis '(�� �� �� ��)
    '(
    �����ʤ���ɽ���ĤȤ��������ʡ��乥Ĵ�Ǥ������ʤ��ο����Х鿧�Ǥ��͡�
    �����ʤ���ɽ���ĤȤ��������ʡ��ʤ��ʤ��Τ�ΤǤ����򹯤˵���Ĥ��Ƴڤ�������������ޤ��礦��
    �����ʤ���ɽ���ĤȤ��������ʡ������񤷤���ΤǤ�����ϫ�����Ȼפ��ޤ��������äȼ¤ꤢ������Ǥ��礦��
    �����ʤ���ɽ���ĤȤ��������ʡ��񤷤���ΤǤ�����������ζ�ϫ�򤵤�뤳�ȤȻפ��ޤ�����������������Τ�������Ȥʤ�Ǥ��礦��
    )
    )))
    (cdr (assoc mark comment))
  )
)
;ŷ�ʥ�����
(defun tenkaku-comment (mark)
  (let ((comment (pairlis '(�� �� �� ��)
    '(
    ���ʤ��βȷϤ�����줿��̿�ϡ��乥Ĵ�ʤ褦�Ǥ�����²�����ڤˤ��ޤ��礦��ǯϷ���ƾ��������������ȤǤ��礦��
    ���ʤ��βȷϤ�����줿��̿�ϡ��ʤ��ʤ��Τ�ΤǤ���ǯ��Ȥ�ФȤ�ۤɱ����ޤ�äƤ���Ǥ��礦��
    ���ʤ��βȷϤ�����줿��̿�ϡ������񤷤���ΤǤ���Ǻ�߻����Ǥ����顢������ȿ�������̤��ޤ��礦��
    ���ʤ��βȷϤ�����줿��̿�ϡ��񤷤���ΤǤ�������䤪�Ż��Ƕ�ϫ�򤵤�뤫�⤷��ޤ��󤬡����ζ�ϫ���䤬�Ƽ¤뤳�ȤǤ��礦��
    )
    )))
    (cdr (assoc mark comment))
  )
)

;�ͳʥ�����
(defun jinkaku-comment (mark)
  (let ((comment (pairlis '(�� �� �� ��)
    '(
    ���ʤ��Ͽ������溢�ˡ������餷���и��򤹤뤳�ȤǤ��礦�����ʤ��ˤȤäƤβ�������⤷��ޤ���
    ���ʤ��Ͽ������溢�ˡ���Ĵ����������뤳�Ȥ��Ǥ��ޤ�������Ū������������ΤǤ���
    ���ʤ��Ͽ������溢�ˡ�������ϫ����뤫�⤷��ޤ��󡣤�����������ʤ���ϫ�Ϥ���ޤ���
    ���ʤ��Ͽ������溢�ˡ��������פ��򤹤뤫�⤷��ޤ��󡣤����������λפ��Ϥ䤬�Ʋ֤Ȥʤ�¤�Ĥ��뤳�ȤǤ��礦��
    )
    )))
    (cdr (assoc mark comment))
  )
)

;�ϳʥ�����
(defun chikaku-comment (mark)
  (let ((comment (pairlis '(�� �� �� ��)
    '(
    �㤤���ˤϡ��ǹ���Ľդ����ä��ȿ仡�Ǥ��ޤ�������˭���ʷи�����ο����˳褭�Ƥ��ޤ���
    �㤤���ˤϡ��褤�Ľդ����줿���Ȥ����ƤȤ�ޤ������δ��Ф�������뤳�Ȥʤ����Ϥ�³�����뤫������Ǥ���
    �㤤���ˤϡ�������ϫ�򤵤줿���⤷��ޤ��󡣤������������������ΤΤ褵�Ǿ���ڤä����ȤȻפ��ޤ���
    �㤤���ˤϡ����Ѥʶ�ϫ�򤵤줿���ȤȻפ��ޤ��������������ζ�ϫ���������ھ�Ȥʤꡢ���ʤ��μ����θ��ˤ�ʤäƤ��ޤ���
    )
    )))
    (cdr (assoc mark comment))
  )
)

;���ʥ�����
(defun gaikaku-comment (mark)
  (let ((comment (pairlis '(�� �� �� ��)
    '(
    �ʹִط��򸫤볰�ʤ�ɽ���Τϡ����ʤ��οʹִط���˭�����Ǥ������Ѥ˷äޤ�Ƥ��ơ��礤�ʤ�̣������ˤ��ʤ����äƤ��ޤ���
    �ʹִط��򸫤볰�ʤ�ɽ���Τϡ����ʤ��οʹִط��ΰ������Ǥ������ʤ�������夤�Ƽ���ο�ã����������ꡢ����޽���������§�äư��ꤷ��������������Ǥ��礦��
    �ʹִط��򸫤볰�ʤ�ɽ���Τϡ����ʤ��οʹִط��ζ�ϫ�Ǥ������ʤ��Ϻ��ΰ��ꤷ���ʹִط����ۤ��뤿��ˡ���ϫ���Ƥ��뤫�⤷��ޤ��󡣤�����������˭���ʿʹִط����ۤ����Ȥ��������Ƥ��뤢�ʤ��ϡ��������пʹط�����äƤ���Ȥ�����ޤ���
    �ʹִط��򸫤볰�ʤ�ɽ���Τϡ����ʤ����пʹط��Ǥζ�ϫ�̤Ǥ��������ǤϤ����ʤ��ͤȡ������򤵤�Ƥ��뤫�⤷��ޤ��󡣤�����������Ϥ䤬�Ƥ��ʤ��Τ褭��̤ؤȷҤ���ΤǤ���
    )
    )))
    (cdr (assoc mark comment))
  )
)



(defun get-num-lucky(num)
  (cond
    ((= num 1) 	"��ã����") 	((= num 11) 	"�޽ա���") 	((= num 21) 	"Ƭ�Ρ���") 	((= num 31) 	"Ƭ�Ρ���") 	((= num 41) 	"���ϡ���") 	((= num 51) 	"��ա���")
    ((= num 2) 	"ư�ɡ���") 	((= num 12) 	"���ޡ���") 	((= num 22) 	"���塡��") 	((= num 32) 	"ѧ������") 	((= num 42) 	"¿�ݡ���") 	((= num 52) 	"��������")
    ((= num 3) 	"��˾����") 	((= num 13) 	"�͵�����") 	((= num 23) 	"Ƭ�Ρ���") 	((= num 33) 	"Ƭ�Ρ���") 	((= num 43) 	"��Ω����") 	((= num 53) 	"ɽ΢����")
    ((= num 4) 	"���졡��") 	((= num 14) 	"��ǡ����") 	((= num 24) 	"��������") 	((= num 34) 	"��ž����") 	((= num 44) 	"�ٺ顡��") 	((= num 54) 	"��������")
    ((= num 5) 	"ʡ������") 	((= num 15) 	"��˾����") 	((= num 25) 	"���ҡ���") 	((= num 35) 	"���ݡ���") ((= num 	45) 	"��������") 	((= num 55) 	"���񡡡�")
    ((= num 6) 	"ŷ������") 	((= num 16) 	"��˾����") 	((= num 26) 	"���𡡡�") 	((= num 36) 	"��ͺ����") 	((= num 46) 	"��������") 	((= num 56) 	"�öˡ���")
    ((= num 7) 	"��Ω����") 	((= num 17) 	"���ҡ���") 	((= num 27) 	"��Ω����") 	((= num 37) 	"��Ω����") 	((= num 47) 	"���֡���") 	((= num 57) 	"�Ƶ�����")
    ((= num 8) 	"��������") 	((= num 18) 	"�䵤����") 	((= num 28) 	"���񡡡�") 	((= num 38) 	"���ء���") 	((= num 48) 	"���ա���") 	((= num 58) 	"�Ƶ�����")
    ((= num 9) 	"�ն�����") 	((= num 19) 	"�㳲����") 	((= num 29) 	"���š���") 	((= num 39) 	"Ƭ�Ρ���") 	((= num 49) 	"��ž����") 	((= num 59) 	"���ڡ���")
    ((= num 10) 	"�Զ�����") 	((= num 20)	"���񡡡�") 	((= num 30) 	"���𡡢�") 	((= num 40) 	"���𡡢�") 	((= num 50) 	"���ࡡ��") 	((= num 60) 	"�Ź�����")
    (t "(����` )")
  )
)

;ʸ����λ��ꤷ�����֤ˤ���ʸ���򥷥�ܥ벽����
(defun convert-str2sym-with-num (str num)
  (read-from-string (format nil "~A" (aref str num)) )
)

;ʸ����λ��ꤷ�����֤δ����β�������
(defun get-count-str-kanji (str num)
  (cdr (assoc (convert-str2sym-with-num str num) kanjicnt))
)

;�褯�����ץߥ�����ΤǤ�餮���б�
(defun laod (fn)
  (laod fn)
)

;last.lsp������
(defun l ()
  (load "last.lsp")
)
