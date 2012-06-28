;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; group and customizations
(defgroup erl-custom nil
  "Additions to Emacs Erlang mode. Mostly new templates."
  :group 'convenience)

(defcustom erl-custom-project-directories
  (list "/home/inaimathi/projects"
	"/home/inaimathi/projects-work/erlang")
  "A list of directories that contain Erlang projects you may want to specify as dependencies"
  :group 'erl-custom)

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
   (insert "exclude = .git .gitignore *~ docs *org *csv *.log *.dump Mnesia* src/*
dependencies = ~/projects-work/erlang/common

### Erlang eval shortcuts
erl_start = -eval 'application:start(sasl), application:load(" project-name "), application:start(" project-name ").'

erl_gen_rel = -eval '{_, _, Version} = lists:keyfind(" project-name ", 1, application:loaded_applications()), Rel = {release, {\"" project-name "_rel\", Version}, {erts, erlang:system_info(version)}, lists:map(fun ({App, _Desc, Ver}) -> {App, Ver} end, application:loaded_applications())}, file:write_file(\"rel/" project-name "-\" ++ Version ++ \".rel\", io_lib:format(\"~p.\", [Rel])).'

erl_build = -eval '{_, _, V} = lists:keyfind(" project-name ", 1, application:loaded_applications()), N = \"rel/" project-name "-\" ++ V, systools:make_script(N, [local]), systools:make_tar(N), os:cmd(lists:append([\"mkdir \", N, \"; tar -xzf \", N, \".tar.gz -C \", N])).'

erl_stop = -eval 'init:stop().'

### Rules
all:
	erlc -Wf -o ebin/ src/*erl
	cp src/*app ebin/

install:
	apt-get install screen erlang libmagickwand-dev python-setuptools
	easy_install erlport

gen-rel:
	erl -pa ebin $(erl_start) $(erl_gen_rel) $(erl_stop)

gen-build: gen-rel
	erl -pa ebin $(erl_start) $(erl_build) $(erl_stop)

mnesia-create:
	erl -eval 'mnesia:create_schema([node()]), init:stop().'

start: 
	screen -S " project-name " erl -pa ebin -name " project-name "@127.0.1.1 $(erl_start)

attach:
	screen -r " project-name "

clean:
	rm ebin/*")))

(defun erl-custom-template-gitignore ()
  "Creates a basic .gitignore file"
  (interactive)
  (with-local-file
   ".gitignore"
   (insert "*~
*.beam
Mnesia*
*dump
*log
*.pyc")))

(defun erl-custom-template-rel (project-name release-name version)
  "Creates a .rel file"
  (interactive
   (list (erl-custom-get-project-name)
	 (read-string "Release Name: ")
	 (read-string "Release Version: ")))
  (with-local-file
   (concat project-name "-" version ".rel")
   (insert "{release, {\""project-name"_rel\", \""release-name"\"},
 {erts, \"5.3\"},
 [{kernel, \"2.15.1\"},
  {stdlib, \"1.18.1\"},
  {sasl, \"2.2.1\"},
  {"project-name", \"" version "\"}]}.")))

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
  {modules, [" project-name "_app, " project-name "_sup, " comps "]},
  {registered, [" comps "]},
  {applications, [kernel, stdlib, sasl]},
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

start(_Type, StartArgs) -> " project-name "_sup:start_link(StartArgs).
stop(_State) -> ok.")))

(defun erl-custom-template-supervisor (supervisor-name module-names)
  "Given a [supervisor-name] and list of [module-name]s, creates the basic `_sup.erl` file."
  (interactive 
   (list (erl-custom-get-project-name)
	 (erl-custom-get-module-names)))
  (with-local-file
   (concat supervisor-name "_sup.erl")
   (insert "-module(" supervisor-name "_sup).
-behavior(supervisor).

-export([start/0, start_for_testing/0, start_link/1, init/1]).

-define(CHILD(I, Type), {I, {I, start, []}, permanent, 10000, Type, [I]}).

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
		(concat "?CHILD(" c ", worker)"))
	      module-names
	      ",\n	   ") "]}}.")))

(defun erl-custom-template-erlport (module-name)
  "Takes a module name and creates corresponding .erl and .py files for a bridge between the two languages."
  (interactive "MModule name: ")
  (with-local-file
   (concat module-name ".py")
   (insert "from erlport import Port, Protocol, String
        
class " (capitalize module-name) "Protocol(Protocol):
    def handle_hello(self, name):
        return \"Hello, %s\" % String(name)

if __name__ == \"__main__\":
    " (capitalize module-name) "Protocol().run(Port(packet=4, use_stdio=True))"))
  (with-local-file
   (concat module-name ".erl")
   (insert "-module(" module-name ").
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([hello/1]).

hello(Name) -> gen_server:call(?MODULE, {hello, Name}).

handle_call({'EXIT', _Port, Reason}, _From, _State) ->
    exit({port_terminated, Reason});
handle_call(Message, _From, Port) ->
    port_command(Port, term_to_binary(Message)),
    receive
	{State, {data, Data}} -> 
	    {reply, binary_to_term(Data), State}
    after 6000 -> 
	    exit(timeout)
    end.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, open_port({spawn, \"python -u " module-name ".py\"}, [{packet, 4}, binary, use_stdio])}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> State ! {self(), close}, ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.")))

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

handle_call(Message, _From, State) -> 
    {reply, {you_sent, Message}, State}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
")))

(defun erl-custom-template-model_server (module-name)
  "Given a [module-name], generates a MINIMAL gen_server module, and includes some model-related files."
  (interactive "MModule Name: ")
  (let ((singular (substring module-name 0 (- (length module-name) 1))))
    (with-local-file
     (concat module-name ".erl")
     (insert "-module(" module-name ").
-behaviour(gen_server).
-include_lib(\"stdlib/include/qlc.hrl\").
-include_lib(\"model.hrl\").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

add(" (capitalize singular) "Name) -> gen_server:call(?MODULE, {insert, #" singular "{id=now(), name=" (capitalize singular) "Name}}).
list() -> gen_server:call(?MODULE, list).
find(" (capitalize singular) "Id) ->
    [Rec] = db:do(qlc:q([X || X <- mnesia:table(" singular "), X#" singular ".id =:= " (capitalize singular) "Id])),
    Rec.

handle_call({insert, Record}, _From, State) ->
    {reply, db:atomic_insert(Record), State};
handle_call(list, _From, State) -> 
    {reply, db:do(qlc:q([X || X <- mnesia:table(" singular ")])), State}.

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}."))))

(defun erl-custom-template-project (project-name description module-names)
  "Takes a [project-name], [description] and list of initial [module-names].
Creates a project folder named [project-name] in the current directory,
initializes it as a git repository, and generates .gitignore, Makefile, subfolders and basic Erlang project files."
  (interactive (erl-custom-get-interactive))
  (let ((cur-dir default-directory)
	(dirname (concat default-directory (replace-regexp-in-string "_" "-" project-name))))
    (progn (mkdir dirname)
	   (shell-command (concat "git init " dirname))

	   (cd dirname)
	   (mkdir "src")
	   (mkdir "include")
	   (mkdir "priv")
	   (mkdir "rel")
	   (mkdir "ebin")
	   (erl-custom-template-makefile project-name)
	   (erl-custom-template-gitignore)
	   (erl-custom-template-notes.org)
	   (erl-custom-template-readme project-name description module-names)

	   (cd "src")
	   (erl-custom-template-app project-name description module-names)
	   (erl-custom-template-app.erl project-name)
	   (erl-custom-template-supervisor project-name module-names)
	   (mapc #'erl-custom-template-gen_server module-names)
	   (cd cur-dir))))

(provide 'erl-custom)