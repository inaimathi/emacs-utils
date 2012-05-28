;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; group and customizations
(defgroup erl-custom nil
  "Additions to Emacs Erlang mode. Mostly new templates."
  :group 'convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utility functions/macros for the generators
(defmacro with-local-file (file-name &rest body)
  "Takes a [file-name] and [body.]
Creates a buffer called [file-name] with `generate-new-buffer`, runs [body] with it as the `current-buffer`, then writes the result to [file-name].
Prompts if a file named [file-name] already exists in the specified directory."
  `(let* ((file-name ,file-name)
	  (buf (generate-new-buffer file-name)))
     (with-current-buffer buf
       ,@body
       (write-file file-name t))))

(defun erl-custom-get-project-name () 
  (replace-regexp-in-string "[ -]" "_" (read-string "Project Name: ")))
(defun erl-custom-get-module-names () 
  (let ((res (completing-read-multiple "Initial Module(s): " nil)))
    (if (every (lambda (s) (string= "" s)) res) (list "foo") res)))
(defun erl-custom-get-interactive ()
  (list (erl-custom-get-project-name)
	(read-string "Project Description: ")
	(erl-custom-get-module-names)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; generation functions
(defun erl-custom-template-readme (project-name description module-names)
  "Takes a [project-name] and [module-names]. 
Creates a README.md with appropriate headings."
  (interactive (erl-custom-get-interactive))
  (with-local-file
   "README.md"
   (insert "# " project-name 
	   "\n*" description "*"
	   "\n\n\n### "
	   (mapconcat 'identity module-names "\n\n### "))))

(defun erl-custom-template-notes.org ()
  "Creates a notes.org file in the current directory for my personal annotation."
  (interactive)
  (with-local-file
   "notes.org"
   (insert "
* Dependencies
  Record any external packages that need to be installed for this project here.

* To Dos
  Any outstanding tasks or notes go here

* Notes
  Any other thoughts about the project go here
")))

(defun erl-custom-template-makefile (project-name)
  "Generates a basic Makefile for an Erlang project.
Needs [project-name] because it creates a `run` task that starts a named node running the project."
  (interactive (list (erl-custom-get-project-name)))
  (with-local-file 
   "Makefile"
   (insert "all: *.erl 
	erlc -W *erl

run: 
	erl -name " project-name "@127.0.1.1 -eval 'application:load("project-name").' -eval 'application:start("project-name").'

clean:
	rm *beam")))

(defun erl-custom-template-gitignore ()
  "Creates a basic .gitignore file that ignores `*~` and `*.beam`."
  (interactive)
  (with-local-file
   ".gitignore"
   (insert "*~
*.beam")))

(defun erl-custom-template-app (project-name description module-names)
  "Takes [project-name], [description] and [module-names] (a list of initial modules for the project), and creates the appropriate basic .app file."
  (interactive (erl-custom-get-interactive))
  (let ((filename (concat project-name ".app"))
	(comps (mapconcat 'identity module-names ", ")))
    (with-local-file
     filename
     (insert "{application, " project-name ",
 [{description, \"" description "\"},
  {vsn, \"1.0\"},
  {modules, [" project-name "_app, " project-name "_supervisor, " comps "]},
  {registered, [" comps "]},
  {applications, [kernel, stdlib]},
  {mod, {" project-name "_app, []}},
  {start_phases, []}]}."))))

(defun erl-custom-template-app.erl (project-name)
  "Creates a `_app.erl` file for the given [project-name]."
  (interactive (list (erl-custom-get-project-name)))
  (with-local-file
   (concat project-name "_app.erl")
   (insert "-module(" project-name "_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> " project-name "_supervisor:start_link(StartArgs).
stop(_State) -> ok.")))

(defun erl-custom-template-supervisor (supervisor-name module-names)
  "Given a [supervisor-name] and list of [module-name]s, creates the basic `_supervisor.erl` file."
  (interactive 
   (list (erl-custom-get-project-name)
	 (erl-custom-get-module-names)))
  (with-local-file
   (concat supervisor-name "_supervisor.erl")
   (insert "-module(" supervisor-name "_supervisor).
-behavior(supervisor).

-export([start/0, start_for_testing/0, start_link/1, init/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_for_testing() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([]) ->
    {ok, {{one_for_one, 3, 10},
	  [" (mapconcat 
	      (lambda (c)
		(concat "{tag_" c ", {" c ", start, []}, permanent, 10000, worker, [" c "]}"))
	      module-names
	      ",\n	   ") "]}}.")))

(defun erl-custom-template-gen_server (module-name)
  "Given a [module-name], generates a MINIMAL gen_server module."
  (interactive "MModule Name: ")
  (with-local-file
   (concat module-name ".erl")
   (insert "-module(" module-name ").
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

handle_call(Message, _From, State) -> {reply, Message, State}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
")))

(defun erl-custom-template-project (project-name description module-names)
  "Takes a [project-name], [description] and list of initial [module-names].
Creates a project folder named [project-name] in the current directory,
initializes it as a git repository, and generates .gitignore, Makefile, and basic Erlang project files."
  (interactive (erl-custom-get-interactive))
  (let ((dirname (concat default-directory (replace-regexp-in-string "_" "-" project-name))))
    (progn (mkdir dirname)
	   (shell-command (concat "git init " dirname))
	   (git-status dirname)
	   
	   (erl-custom-template-makefile project-name)
	   (erl-custom-template-gitignore)
	   (erl-custom-template-notes.org)
	   (erl-custom-template-readme project-name description module-names)

	   (erl-custom-template-app project-name description module-names)
	   (erl-custom-template-app.erl project-name)
	   (erl-custom-template-supervisor project-name module-names)
	   (mapc #'erl-custom-template-gen_server module-names))))

(provide 'erl-custom)