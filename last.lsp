
;フィボナッチ数列を計算する関数。処理に時間がかかるので、waitを入れるために使う
(defun fibo (x)
  (if (or (= 0 x) (= 1 x))
      1
      (+ (fibo (- x 1)) (fibo (- x 2)))))

;画面を消去する関数
(defun screen-clear(x)
  (progn
  (dotimes (i x)
    (print '　)
  )
  (fibo 20)
  (print '　)
  )
)

;アニメーションさせる関数。dataリストの中身を、x回表示する
(defun anime(x data)
  (dotimes (i x)
    (progn
      (princ (nth (mod i (length data)) data) )
      (screen-clear 19)
    )
  )
)

;ファイルから読み込んでリストにする関数
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

;ファイルから読み込んだデータをアニメ化する
(defun file-anime (file num)
  (anime num (read-file-list file))
)
;マルチライン対応のファイルから読み込んだデータをアニメ化する
(defun file-anime-m (file num)
  (anime-m num (read-file-list-m file "＊"))
)

;漢字データベースの読み込み
(load "kanji.lsp")

;占い本体
(defun uranai ()
  (progn
    (title)
    (2linemes "This program could be running on EUC-JP")
    (commandmes "名字を入力してください。 (ex. 山田)")
    (setq fam (string (read)))
    (commandmes "名前を入力してください。 (ex. 五郎)")
    (setq las (string (read)))
    (file-anime "calculating.anime" 77)
    (2linemes (format nil "~A ~Aさん" fam las))
    (returnn 2)
    (2linemes (format nil "あなたの総格は~D画：~A" (soukaku fam las) (get-num-lucky (soukaku fam las))))
    (2linemes (format nil "あなたの天格は~D画：~A" (tenkaku fam) (get-num-lucky (tenkaku fam))))
    (2linemes (format nil "あなたの人格は~D画：~A" (jinkaku fam las) (get-num-lucky (jinkaku fam las))))
    (2linemes (format nil "あなたの地格は~D画：~A" (chikaku las) (get-num-lucky (chikaku las))))
    (2linemes (format nil "あなたの外格は~D画：~A" (gaikaku fam las) (get-num-lucky (gaikaku fam las))))
    (returnn 2)
    (princ (soukaku-comment (convert-str2sym-with-num (get-num-lucky (soukaku fam las)) 3) ) )
    (princ (tenkaku-comment (convert-str2sym-with-num (get-num-lucky (tenkaku fam)) 3) ) )
    (princ (jinkaku-comment (convert-str2sym-with-num (get-num-lucky (jinkaku fam las)) 3) ) )
    (princ (chikaku-comment (convert-str2sym-with-num (get-num-lucky (chikaku las)) 3) ) )
    (princ (gaikaku-comment (convert-str2sym-with-num (get-num-lucky (gaikaku fam las)) 3) ) )
    (returnn 4)
  )
)

;改行付きのprinc
(defun 2linemes (mes)
  (progn
  (princ mes)
  (terpri)
  )
)
;>って付けてる2linemes
(defun commandmes (mes)
  (progn
  (2linemes mes)
  (princ ">")
  )
)
;指定した回数改行する関数
(defun returnn (num)
  (progn
  (dotimes (i (1- num))
    (terpri)
  )
    (princ '　)
  )
)

(defun title()
  (progn
    (princ "　　　　 ∧_,,∧")
    (terpri)
    (princ ".　/＼. (｀･ω･´) ／ヽ　")
    (terpri)
    (princ "　| ● ⊂　　　⊃  ● |　　　　　運気上昇うらない！！！")
    (terpri)
    (princ "　ヽ／　/ 　　く　＼ /")
    (terpri)
    (princ "　　　　（ノ⌒ヽ）")
    (terpri)
  )
)

;総格
(defun soukaku (fam las)
  (+ (tenkaku fam) (chikaku las))
)
;天格
(defun tenkaku (fam &optional (pos 0) (sum 0))
  (if (= pos (length fam)) sum
    (tenkaku fam (1+ pos) (+ sum (get-count-str-kanji fam pos) ) )
))
;人格
(defun jinkaku (fam las)
  (+ (get-count-str-kanji fam (1- (length fam))) (get-count-str-kanji las 0) )
)
;地格
(defun chikaku (las &optional (pos 0) (sum 0))
  (if (= pos (length las)) sum
    (chikaku las (1+ pos) (+ sum (get-count-str-kanji las pos) ) )
))
;外格
(defun gaikaku (fam las)
  (+ (get-count-str-kanji fam 0) (get-count-str-kanji las (1- (length las))) )
)




;総格コメント
(defun soukaku-comment (mark)
  (let ((comment (pairlis '(◎ ○ △ ●)
    '(
    　あなたの表看板とも言える総格、絶好調です。あなたの人生バラ色ですね。
    　あなたの表看板とも言える総格、なかなかのものです。健康に気をつけて楽しい人生を送りましょう。
    　あなたの表看板とも言える総格、少し難しいものです。苦労されると思いますが、きっと実りある人生でしょう。
    　あなたの表看板とも言える総格、難しいものです。たくさんの苦労をされることと思いますが、それだけ輝きのある人生となるでしょう。
    )
    )))
    (cdr (assoc mark comment))
  )
)
;天格コメント
(defun tenkaku-comment (mark)
  (let ((comment (pairlis '(◎ ○ △ ●)
    '(
    あなたの家系に秘められた運命は、絶好調なようです。家族を大切にしましょう。年老いて尚輝きを増すことでしょう。
    あなたの家系に秘められた運命は、なかなかのものです。年をとればとるほど運がまわってくるでしょう。
    あなたの家系に秘められた運命は、少し難しいものです。悩み事ができたら、きちんと身内に相談しましょう。
    あなたの家系に秘められた運命は、難しいものです。家庭やお仕事で苦労をされるかもしれませんが、その苦労がやがて実ることでしょう。
    )
    )))
    (cdr (assoc mark comment))
  )
)

;人格コメント
(defun jinkaku-comment (mark)
  (let ((comment (pairlis '(◎ ○ △ ●)
    '(
    あなたは人生の中頃に、素晴らしい経験をすることでしょう。あなたにとっての黄金期かもしれません。
    あなたは人生の中頃に、好調な生活を送ることができます。安定的な生活を送れるのです。
    あなたは人生の中頃に、少し苦労されるかもしれません。しかし、報われない苦労はありません。
    あなたは人生の中頃に、厳しい思いをするかもしれません。しかし、その思いはやがて花となり実をつけることでしょう。
    )
    )))
    (cdr (assoc mark comment))
  )
)

;地格コメント
(defun chikaku-comment (mark)
  (let ((comment (pairlis '(◎ ○ △ ●)
    '(
    若い頃には、最高な青春を送ったと推察できます。その豊かな経験が後の人生に活きています。
    若い頃には、よい青春を送れたことが見てとれます。その感覚を引きずることなく努力を続けられるかが課題です。
    若い頃には、少し苦労をされたかもしれません。しかし、持ち前の要領のよさで乗り切ったことと思います。
    若い頃には、大変な苦労をされたことと思います。しかし、その苦労が人生の土壌となり、あなたの自信の源にもなっています。
    )
    )))
    (cdr (assoc mark comment))
  )
)

;外格コメント
(defun gaikaku-comment (mark)
  (let ((comment (pairlis '(◎ ○ △ ●)
    '(
    人間関係を見る外格に表れるのは、あなたの人間関係の豊かさです。大変に恵まれていて、大いなる味方が常にあなたを守っています。
    人間関係を見る外格に表れるのは、あなたの人間関係の安定性です。あなたは落ち着いて周りの人達と生活を送り、相互扶助の精神に則って安定した生活を手に入れるでしょう。
    人間関係を見る外格に表れるのは、あなたの人間関係の苦労です。あなたは今の安定した人間関係を構築するために、苦労しているかもしれません。しかし、総じて豊かな人間関係を築くことに成功しているあなたは、幸せな対人関係を持っているとも言えます。
    人間関係を見る外格に表れるのは、あなたの対人関係での苦労面です。一筋縄ではいかない人と、やり取りをされているかもしれません。しかし、それはやがてあなたのよき結果へと繋がるのです。
    )
    )))
    (cdr (assoc mark comment))
  )
)



(defun get-num-lucky(num)
  (cond
    ((= num 1) 	"栄達　◎") 	((= num 11) 	"迎春　◎") 	((= num 21) 	"頭領　◎") 	((= num 31) 	"頭領　◎") 	((= num 41) 	"実力　◎") 	((= num 51) 	"注意　○")
    ((= num 2) 	"動揺　●") 	((= num 12) 	"挫折　●") 	((= num 22) 	"薄弱　●") 	((= num 32) 	"僥倖　◎") 	((= num 42) 	"多芸　△") 	((= num 52) 	"功利　○")
    ((= num 3) 	"希望　○") 	((= num 13) 	"人気　○") 	((= num 23) 	"頭領　◎") 	((= num 33) 	"頭領　○") 	((= num 43) 	"独立　△") 	((= num 53) 	"表裏　△")
    ((= num 4) 	"困苦　●") 	((= num 14) 	"不如　●") 	((= num 24) 	"興産　○") 	((= num 34) 	"変転　△") 	((= num 44) 	"遅咲　○") 	((= num 54) 	"破兆　●")
    ((= num 5) 	"福寿　○") 	((= num 15) 	"徳望　○") 	((= num 25) 	"英敏　○") 	((= num 35) 	"技芸　△") ((= num 	45) 	"順風　○") 	((= num 55) 	"機会　●")
    ((= num 6) 	"天徳　○") 	((= num 16) 	"衆望　◎") 	((= num 26) 	"波乱　●") 	((= num 36) 	"英雄　△") 	((= num 46) 	"破船　●") 	((= num 56) 	"消極　●")
    ((= num 7) 	"孤立　△") 	((= num 17) 	"権威　△") 	((= num 27) 	"孤立　△") 	((= num 37) 	"独立　○") 	((= num 47) 	"開花　○") 	((= num 57) 	"再起　△")
    ((= num 8) 	"根気　○") 	((= num 18) 	"剛気　○") 	((= num 28) 	"遭難　●") 	((= num 38) 	"技学　△") 	((= num 48) 	"軍師　○") 	((= num 58) 	"再起　△")
    ((= num 9) 	"逆境　●") 	((= num 19) 	"障害　●") 	((= num 29) 	"知謀　○") 	((= num 39) 	"頭領　○") 	((= num 49) 	"変転　△") 	((= num 59) 	"停滞　●")
    ((= num 10) 	"不遇　●") 	((= num 20)	"災厄　●") 	((= num 30) 	"波乱　△") 	((= num 40) 	"波乱　△") 	((= num 50) 	"衰退　●") 	((= num 60) 	"暗黒　●")
    (t "(´Д` )")
  )
)

;文字列の指定した位置にある文字をシンボル化する
(defun convert-str2sym-with-num (str num)
  (read-from-string (format nil "~A" (aref str num)) )
)

;文字列の指定した位置の漢字の画数を取得
(defun get-count-str-kanji (str num)
  (cdr (assoc (convert-str2sym-with-num str num) kanjicnt))
)

;よくタイプミスするのでゆらぎに対応
(defun laod (fn)
  (laod fn)
)

;last.lspをリロード
(defun l ()
  (load "last.lsp")
)
