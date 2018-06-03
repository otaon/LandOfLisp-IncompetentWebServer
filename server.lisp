(defun http-char (c1 c2 &optional (default #\Space))
  "16進数で表されたASCIIコードをデコードする
   c1: 2桁目の数値となる文字
   c2: 1桁目の数値となる文字"
  ;; 16進数の文字列を整数へと変換する
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16            ; 数の基数を指定
                :junk-allowed t)))	 ; 数値の解釈を失敗した時、エラー通知ではなくnilを返す
    ;; 整数への変換が成功したら、そのコードに対応した文字を返す
    ;; 整数への変換が失敗したら、default値を返す
    (if code
        (code-char code)
        default)))


(defun decode-param-en (s)
  "httpエスケープされているリクエストパラメータをデコードする(ASCIIコードのみ対応)"
  ;; f: 文字のリストを再帰的に処理するローカル関数
  (labels ((f (lst)
              (when lst
                ;; 文字が%なら、次に2桁の16進数で表されるASCIIコードをデコードする
                ;; 文字が+なら、空白文字として解釈する
                ;; 他の文字なら、そのまま出力する
                (case (car lst)
                    ;; リストの先頭の文字を処理し、残りの文字列（処理済み）と組み合わせる
                    (#\% (cons (http-char (cadr lst) (caddr lst))
                               (f (cdddr lst))))
                    (#\+ (cons #\space
                               (f (cdr lst))))
                    (otherwise (cons (car lst)
                               (f (cdr lst))))))))
    ;; リストの要素を文字列として結合する
    (coerce (f (coerce s 'list)) 'string)))


;; 文字ごとではなく、バイトごとにデコードする(URLの正式なエンコーディング準拠)
(defun http-byte (c1 c2 &optional (default #\Space))
  "16進数で表された文字をバイト数値にデコードする
   c1: 2桁目の数値となる文字
   c2: 1桁目の数値となる文字"
  ;; 16進数の文字列を整数へと変換する
  (let ((code (parse-integer
                (coerce (list (code-char c1) (code-char c2)) 'string)
                :radix 16            ; 数の基数を指定
                :junk-allowed t)))	 ; 数値の解釈を失敗した時、エラー通知ではなくnilを返す
    ;; 整数への変換が成功したら、そのコードに対応したバイト数値を返す
    ;; 整数への変換が失敗したら、default値を返す
    (or code default)))


(defun decode-param-ja (s)
  "httpエスケープされているリクエストパラメータをデコードする(マルチバイト文字対応)"
  ;; f: 文字のリストを再帰的に処理するローカル関数
  (labels ((f (lst)
              (when lst
                ;; 文字が%なら、次に2桁の16進数で表されるASCIIコードをデコードする
                ;; 文字が+なら、空白文字として解釈する
                ;; 他の文字なら、そのまま出力する
                (case (car lst)
                    ;; リストの先頭の文字を処理し、残りの文字列（処理済み）と組み合わせる
                    (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
                                             (f (cdddr lst))))
                    (#.(char-code #\+) (cons #.(char-code #\space)
                                             (f (cdr lst))))
                    (otherwise (cons (car lst)
                               (f (cdr lst))))))))
    ;; リストの要素を文字列として結合する
    (ext:convert-string-from-bytes
      (coerce (f (coerce (ext:convert-string-to-bytes s charset:utf-8) 'list)) 'vector)
      charset:utf-8)))


(defun decode-param (s)
  (let ((param-enc :ja))
    (cond ((equal param-enc :en) (decode-param-en s))
          ((equal param-enc :ja) (decode-param-ja s)))))


;;; charsetには下記などが使える。
;;; charset:utf-8
;;; charset:euc-jp
;;; charset:shift-jis
;CLISP用設定
;(setf *terminal-encoding* charset:utf-8)


;; **NOTE:** Webサーバで日本語を表示するためには、ソケットの文字エンコーディングも指定する必要がある。
;; `serve`コマンド（後述）を起動する前に、REPL上で次のコマンドを実行すること。
(setf *default-file-encoding* charset:utf-8)


(defun parse-params (s)
  "リクエストパラメータのalistを返す
   s: リクエストパラメータの文字列
   ret: リクエストパラメータのalist"
  (let ((i1 (position #\= s))	; リクエストパラメータ中の=の位置
        (i2 (position #\& s)))  ; リクエストパラメータ中の&の位置
    (cond (i1 (cons	; 名前と値の各コンスセルをコンスする
                (cons (intern (string-upcase (subseq s 0 i1)))	; car部：名前をシンボルに変換したもの
                      (decode-param (subseq s (1+ i1) i2)))		; cdr部：値のhttpエスケープをデコードしたもの
                (and i2 (parse-params (subseq s (1+ i2))))))	; 残りのリクエストパラメータに対して処理
          ((equal s "") nil)	; リクエストパラメータが空になったらリストを閉じるためにnilを返す
          (t s))))	; リクエストパラメータの書式ではない文字列の場合、文字列をそのまま返す


(defun parse-request-line (s)
  "リクエストヘッダのリクエストラインからURLを取り出す
   s: リクエストライン
   ret: url本体部とリクエストパラメータ部とのコンスセル"
  (let* ((url (subseq s
                      (+ 2 (position #\space s))          ; スペース位置から2つ進んだ箇所(`/`の次)
                      (position #\space s :from-end t)))  ; 文字列の後ろから見てスペースのある箇所
         (x (position #\? url)))  ; URL中のリクエストパラメータの開始位置
    (if x    ; リクエストパラメータがある
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))    ; url本体部とリクエストパラメータ部とのコンスセル
        (cons url '()))))    ; url本体部と空リストとのコンスセル


(defun get-header (stream)
  "リクエストヘッダのHTTPヘッダフィールドからリクエストパラメータを返す
   stream: HTTPヘッダフィールド
   ret: リクエストパラメータと値とのコンスセル"
  (let* ((s (read-line stream))  ; 入力ストリームから得た文字列1行分
         (h (let ((i (position #\: s)))  ; コロンの位置
              (when i	; コロンがある場合、コロンを区切りとしたリクエスト名/値のコンスセルを作る
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    ;; コンスセルができたら、残りのリクエストも処理する
    ;; コンスセルができなかったら、それ以降はリクエストは無いなずなので、処理を終わる
    (when h
      (cons h (get-header stream)))))


(defun get-content-params (stream header)
  "リクエストヘッダの後にあるリクエストボディから、パラメータを取り出す
   stream: ストリーム
   header: HTTPヘッダフィールドの連想リスト"
  (let ((length (cdr (assoc 'content-length header))))  ; HTTPヘッダフィールドからコンテンツの長さを取得する
    ;; もしcontent-lengthがHTTPヘッダフィールドにあれば、リクエストパラメータの連想リストを作る
    (when length
      (let ((content (make-string (parse-integer length))))  ; 与えられた長さの文字列を`make-string`で作成する
        (read-sequence content stream)  ; ストリームからデータを読み込んで、contentを満たす
        (parse-params content)))))      ; リクエストパラメータの連想リストを作る


(defun serve (request-handler)
  "request-handler: リクエストハンドラ。解析したリクエストを使う。"
  (let ((socket (socket-server 8080)))  ; サーバのポート番号
    (unwind-protect  ; 例外時にソケットが確実に閉じられるようにする
      (loop (with-open-stream (stream (socket-accept socket))  ; 接続が確立したらソケットオブジェクトをstreamにセットする
              (let* ((url    (parse-request-line (read-line stream)))  ; streamからURLとリクエストパラメータを得る
                     (path   (car url))            ; URLのパス部
                     (header (get-header stream))  ; HTTPヘッダフィールド
                     (params (append (cdr url)     ; URL末尾(GET用)とリクエストボディ(POST用)のリクエストパラメータ
                                     (get-content-params stream header)))
                     (*standard-output* stream))   ; ストリームを標準出力に設定
                (funcall request-handler path header params))))  ; 
      (socket-server-close socket))))


(defun hello-request-handler (path header params)
  "名前を問いかけて、得られたその名前を使って挨拶する
   CAUTION! リクエストパラメータをサニタイズしていないため、WANでの使用不可
   path: URLのパス部分
   header: HTTPヘッダフィールド
   params: URL末尾(GET用)とリクエストボディ(POST用)のリクエストパラメータ
   ret: レスポンスするHTMLドキュメント"
  (declare (ignore header))  ; 本関数ではHTTPヘッダフィールドは無視する
  ;; "/greeting"ページのみ提供する
  (if (equal path "greeting")
      ;; ページが"greeting"ならパラメータに合わせて表示処理を行う
      (let ((name (assoc 'name params)))
        (if (not name)
            ;; パラメータにnameが無ければ、もう一度名前を問いかける
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            ;; パラメータにnameがあれば、挨拶を表示する
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      ;; ページが"greeting"でなければ、要求されたページが無い旨を表示する
      (princ "Sorry... I don't know that page.")))

