(module conjure.client.guile.stdio
  {require {a conjure.aniseed.core
            str conjure.aniseed.string
            nvim conjure.aniseed.nvim
            stdio conjure.remote.stdio
            config conjure.config
            text conjure.text
            mapping conjure.mapping
            client conjure.client
            log conjure.log}
   require-macros [conjure.macros]})

(config.merge
  {:client
   {:guile
    {:stdio
     {:mapping {:start "cs"
                :stop "cS"}
      :command "guile -q"
      :prompt-pattern "\n?scheme@%([%w%-]+%)> "}}}})

(def- cfg (config.get-in-fn [:client :guile :stdio]))

(defonce- state (client.new-state #(do {:repl nil})))

(def buf-suffix ".scm")
(def comment-prefix "; ")

(defn- with-repl-or-warn [f opts]
  (let [repl (state :repl)]
    (if repl
      (f repl)
      (log.append [(.. comment-prefix "No REPL running")]))))

(defn- format-message [msg]
  (str.split (or msg.out msg.err) "\n"))

(defn- display-result [msg]
  (log.append
    (->> (format-message msg)
         (a.filter #(not (= "" $1))))))

(defn eval-str [opts]
  (with-repl-or-warn
    (fn [repl]
      (repl.send
        opts.code
        (fn [msgs]
          (when (and (= 1 (a.count msgs))
                     (= "" (a.get-in msgs [1 :out])))
            (a.assoc-in msgs [1 :out] (.. comment-prefix "Empty result.")))

          (opts.on-result (str.join "\n" (format-message (a.last msgs))))
          (a.run! display-result msgs))
        {:batch? true}))))

(defn eval-file [opts]
  (eval-str (a.assoc opts :code (.. ",require-reloadable " opts.file-path))))

(defn doc-str [opts]
  (eval-str (a.update opts :code #(.. ",doc " $1))))

(defn- display-repl-status [status]
  (let [repl (state :repl)]
    (when repl
      (log.append
        [(.. comment-prefix (a.pr-str (a.get-in repl [:opts :cmd])) " (" status ")")]
        {:break? true}))))

(defn stop []
  (let [repl (state :repl)]
    (when repl
      (repl.destroy)
      (display-repl-status :stopped)
      (a.assoc (state) :repl nil))))

(defn start []
  (if (state :repl)
    (log.append ["; Can't start, REPL is already running."
                 (.. "; Stop the REPL with "
                     (config.get-in [:mapping :prefix])
                     (cfg [:mapping :stop]))]
                {:break? true})
    (a.assoc
      (state) :repl
      (stdio.start
        {:prompt-pattern (cfg [:prompt-pattern])
         :cmd (cfg [:command])

         :on-success
         (fn []
           (display-repl-status :started))

         :on-error
         (fn [err]
           (display-repl-status err))

         :on-exit
         (fn [code signal]
           (when (and (= :number (type code)) (> code 0))
             (log.append [(.. comment-prefix "process exited with code " code)]))
           (when (and (= :number (type signal)) (> signal 0))
             (log.append [(.. comment-prefix "process exited with signal " signal)]))
           (stop))

         :on-stray-output
         (fn [msg]
           (display-result msg))}))))

(defn on-load []
  ;(augroup
  ;  conjure-guile-stdio-bufenter
  ;  (autocmd :BufEnter (.. :* buf-suffix) (viml->fn :enter)))
  (start))

(defn on-filetype []
  (mapping.buf :n :GuileStart (cfg [:mapping :start]) *module-name* :start)
  (mapping.buf :n :GuileStop (cfg [:mapping :stop]) *module-name* :stop))
