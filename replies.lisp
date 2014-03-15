;;; replies.lisp --- 

;;; Copyright  2014 David Vázquez

;; This file is part of cl-ircd.
;;
;; cl-ircd is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; cl-ircd is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-ircd.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-ircd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-docspec-variable (name)
    (loop
       for (from to) in '((#\space #\-) (#\# #\n))
       do (setq name (nsubstitute to from name)))
    (intern (string-upcase name)))

  (defun read-docspec-variable (stream)
    (with-output-to-string (var)
      (loop
         for ch = (read-char stream)
         until (char= ch #\>)
         do (write-char ch var))))

  (defun convert-reply-docspec (string)
    "Given a reply documentation spec, return two values:

- A list of symbols, which are variables that occur in the
  specification. The symbol names are canonalized. See below for some
  examples.

- A list of arguments for the reply. Each argument is in the form
  (FMT ARGS) where FMT is a format-like control string and ARGS are
  symbols from the previous list."
    (when string
      (let ((variable-occurences nil))
        (with-open-stream (stream (make-string-output-stream))
          (with-input-from-string (in string)
            ;; Collect variable ocurrences in the string, replacing
            ;; them by _~a_ format-like placeholders.
            (loop
               for ch = (read-char in nil)
               while ch
               do (if (char= ch #\<)
                      (let ((var (read-docspec-variable in)))
                        (push (intern-docspec-variable var) variable-occurences)
                        (write-string "~a" stream))
                      (write-char ch stream)))
            (setq variable-occurences (nreverse variable-occurences))
            ;; Parse the resulting string as arguments of a IRC
            ;; message. Note it is done _after_ replacing variables,
            ;; because they could contain space and break the parser.
            ;; Each argument is matched with the variables it uses.
            (values (remove-duplicates variable-occurences)
                    (mapcar (lambda (arg)
                              (let ((count (count #\~ arg)))
                                (prog1 (cons arg (subseq variable-occurences 0 count))
                                  (setq variable-occurences (nthcdr count variable-occurences)))))
                            (parse-input-arguments (get-output-stream-string stream))))))))))



(defmacro defreply (code name &optional string)
  (multiple-value-bind (variables format-arguments)
      (convert-reply-docspec string)
    format-arguments
    `(defun ,name (&key ,@variables)
       (declare (ignorable ,@variables))
       (rpl ,(format nil "~3,'0d" code)
             ,@(mapcar (lambda (format-argument)
                         `(format nil "~?"
                                  ,(car format-argument)
                                  (list ,@(cdr format-argument))))
                       format-arguments)))))


(defreply 001 RPL-WELCOME ":Welcome to the Internet Relay Network <nick>!<user>@<host>")
(defreply 002 RPL-YOURHOST ":Your host is <servername>, running version <ver>")
(defreply 003 RPL-CREATED ":This server was created <date>")
(defreply 004 RPL-MYINFO "<servername> <version> <available user modes> <available channel modes>")
(defreply 005 RPL-BOUNCE ":Try server <server name>, port <port number>")
;; (defreply 302 RPL-USERHOST ":*1<reply> *( \" \" <reply> )")
;; (defreply 303 RPL-ISON ":*1<nick> *( \" \" <nick> )")
(defreply 301 RPL-AWAY "<nick> :<away message>")
(defreply 305 RPL-UNAWAY ":You are no longer marked as being away")
(defreply 306 RPL-NOWAWAY ":You have been marked as being away")
(defreply 311 RPL-WHOISUSER "<nick> <user> <host> * :<real name>")
(defreply 312 RPL-WHOISSERVER "<nick> <server> :<server info>")
(defreply 313 RPL-WHOISOPERATOR "<nick> :is an IRC operator")
(defreply 317 RPL-WHOISIDLE "<nick> <integer> :seconds idle")
(defreply 318 RPL-ENDOFWHOIS "<nick> :End of WHOIS list")
;; (defreply 319 RPL-WHOISCHANNELS "<nick> :*( ( \"@\" / \"+\" ) <channel> \" \" )")
(defreply 314 RPL-WHOWASUSER "<nick> <user> <host> * :<real name>")
(defreply 369 RPL-ENDOFWHOWAS "<nick> :End of WHOWAS")
(defreply 321 RPL-LISTSTART)
(defreply 322 RPL-LIST "<channel> <# visible> :<topic>")
(defreply 323 RPL-LISTEND ":End of LIST")
(defreply 325 RPL-UNIQOPIS "<channel> <nickname>")
(defreply 324 RPL-CHANNELMODEIS "<channel> <mode> <mode params>")
(defreply 331 RPL-NOTOPIC "<channel> :No topic is set")
(defreply 332 RPL-TOPIC "<channel> :<topic>")
(defreply 341 RPL-INVITING "<channel> <nick>")
(defreply 342 RPL-SUMMONING "<user> :Summoning user to IRC")
(defreply 346 RPL-INVITELIST "<channel> <invitemask>")
(defreply 347 RPL-ENDOFINVITELIST "<channel> :End of channel invite list")
(defreply 348 RPL-EXCEPTLIST "<channel> <exceptionmask>")
(defreply 349 RPL-ENDOFEXCEPTLIST "<channel> :End of channel exception list")
(defreply 351 RPL-VERSION "<version>.<debuglevel> <server> :<comments>")
;; (352 RPL-WHOREPLY "<channel> <user> <host> <server> <nick>
;;        ( "H" / "G" > ["*"] [ ( "@" / "+" ) ]
;;        :<hopcount> <real name>")
(defreply 315 RPL-ENDOFWHO "<name> :End of WHO list")
;; (353    RPL-NAMREPLY
;;        "( "=" / "*" / "@" ) <channel>
;;         :[ "@" / "+" ] <nick> *( " " [ "@" / "+" ] <nick> ))
(defreply 366 RPL-ENDOFNAMES "<channel> :End of NAMES list")
(defreply 364 RPL-LINKS "<mask> <server> :<hopcount> <server info>")
(defreply 365 RPL-ENDOFLINKS "<mask> :End of LINKS list")
(defreply 367 RPL-BANLIST "<channel> <banmask>")
(defreply 368 RPL-ENDOFBANLIST "<channel> :End of channel ban list")
(defreply 371 RPL-INFO ":<string>")
(defreply 374 RPL-ENDOFINFO ":End of INFO list")
(defreply 375 RPL-MOTDSTART ":- <server> Message of the day - ")
(defreply 372 RPL-MOTD ":- <text>")
(defreply 376 RPL-ENDOFMOTD ":End of MOTD command")
(defreply 381 RPL-YOUREOPER ":You are now an IRC operator")
(defreply 382 RPL-REHASHING "<config file> :Rehashing")
(defreply 383 RPL-YOURESERVICE "You are service <servicename>")
(defreply 391 RPL-TIME "<server> :<string showing server's local time>")
(defreply 392 RPL-USERSSTART ":UserID   Terminal  Host")
(defreply 393 RPL-USERS ":<username> <ttyline> <hostname>")
(defreply 394 RPL-ENDOFUSERS ":End of users")
(defreply 395 RPL-NOUSERS ":Nobody logged in")
(defreply 200 RPL-TRACELINK "Link <version & debug level> <destination> <next server> V<protocol version> <link uptime in seconds> <backstream sendq> <upstream sendq>")
(defreply 201 RPL-TRACECONNECTING "Try. <class> <server>")
(defreply 202 RPL-TRACEHANDSHAKE "H.S. <class> <server>")
(defreply 203 RPL-TRACEUNKNOWN "???? <class> [<client IP address in dot form>]")
(defreply 204 RPL-TRACEOPERATOR "Oper <class> <nick>")
(defreply 205 RPL-TRACEUSER "User <class> <nick>")
(defreply 206 RPL-TRACESERVER "Serv <class> <int>S <int>C <server> <nick!user|*!*>@<host|server> V<protocol version>")
(defreply 207 RPL-TRACESERVICE "Service <class> <name> <type> <active type>")
(defreply 208 RPL-TRACENEWTYPE "<newtype> 0 <client name>")
(defreply 209 RPL-TRACECLASS "Class <class> <count>")
(defreply 261 RPL-TRACELOG "File <logfile> <debug level>")
(defreply 262 RPL-TRACEEND "<server name> <version & debug level> :End of TRACE")
(defreply 211 RPL-STATSLINKINFO "<linkname> <sendq> <sent messages> <sent Kbytes> <received messages> <received Kbytes> <time open>")
(defreply 212 RPL-STATSCOMMANDS "<command> <count> <byte count> <remote count>")
(defreply 219 RPL-ENDOFSTATS "<stats letter> :End of STATS report")
(defreply 242 RPL-STATSUPTIME ":Server Up %d days %d:%02d:%02d")
(defreply 243 RPL-STATSOLINE "O <hostmask> * <name>")
(defreply 221 RPL-UMODEIS "<user mode string>")
(defreply 234 RPL-SERVLIST "<name> <server> <mask> <type> <hopcount> <info>")
(defreply 235 RPL-SERVLISTEND "<mask> <type> :End of service listing")
(defreply 251 RPL-LUSERCLIENT ":There are <integer> users and <integer> services on <integer> servers")
(defreply 252 RPL-LUSEROP "<integer> :operator(s) online")
(defreply 253 RPL-LUSERUNKNOWN "<integer> :unknown connection(s)")
(defreply 254 RPL-LUSERCHANNELS "<integer> :channels formed")
(defreply 255 RPL-LUSERME ":I have <integer> clients and <integer> servers")
(defreply 256 RPL-ADMINME "<server> :Administrative info")
(defreply 257 RPL-ADMINLOC1 ":<admin info>")
(defreply 258 RPL-ADMINLOC2 ":<admin info>")
(defreply 259 RPL-ADMINEMAIL ":<admin info>")
(defreply 263 RPL-TRYAGAIN "<command> :Please wait a while and try again.")
(defreply 401 ERR-NOSUCHNICK "<nickname> :No such nick/channel")
(defreply 402 ERR-NOSUCHSERVER "<server name> :No such server")
(defreply 403 ERR-NOSUCHCHANNEL "<channel name> :No such channel")
(defreply 404 ERR-CANNOTSENDTOCHAN "<channel name> :Cannot send to channel")
(defreply 405 ERR-TOOMANYCHANNELS "<channel name> :You have joined too many channels")
(defreply 406 ERR-WASNOSUCHNICK "<nickname> :There was no such nickname")
(defreply 407 ERR-TOOMANYTARGETS "<target> :<error code> recipients. <abort message>")
(defreply 408 ERR-NOSUCHSERVICE "<service name> :No such service")
(defreply 409 ERR-NOORIGIN ":No origin specified")
(defreply 411 ERR-NORECIPIENT ":No recipient given (<command>)")
(defreply 412 ERR-NOTEXTTOSEND ":No text to send")
(defreply 413 ERR-NOTOPLEVEL "<mask> :No toplevel domain specified")
(defreply 414 ERR-WILDTOPLEVEL "<mask> :Wildcard in toplevel domain")
(defreply 415 ERR-BADMASK "<mask> :Bad Server/host mask")
(defreply 421 ERR-UNKNOWNCOMMAND "<command> :Unknown command")
(defreply 422 ERR-NOMOTD ":MOTD File is missing")
(defreply 423 ERR-NOADMININFO "<server> :No administrative info available")
(defreply 424 ERR-FILEERROR ":File error doing <file op> on <file>")
(defreply 431 ERR-NONICKNAMEGIVEN ":No nickname given")
(defreply 432 ERR-ERRONEUSNICKNAME "<nick> :Erroneous nickname")
(defreply 433 ERR-NICKNAMEINUSE "<nick> :Nickname is already in use")
(defreply 436 ERR-NICKCOLLISION "<nick> :Nickname collision KILL from <user>@<host>")
(defreply 437 ERR-UNAVAILRESOURCE "<nick/channel> :Nick/channel is temporarily unavailable")
(defreply 441 ERR-USERNOTINCHANNEL "<nick> <channel> :They aren't on that channel")
(defreply 442 ERR-NOTONCHANNEL "<channel> :You're not on that channel")
(defreply 443 ERR-USERONCHANNEL "<user> <channel> :is already on channel")
(defreply 444 ERR-NOLOGIN "<user> :User not logged in")
(defreply 445 ERR-SUMMONDISABLED ":SUMMON has been disabled")
(defreply 446 ERR-USERSDISABLED ":USERS has been disabled")
(defreply 451 ERR-NOTREGISTERED ":You have not registered")
(defreply 461 ERR-NEEDMOREPARAMS "<command> :Not enough parameters")
(defreply 462 ERR-ALREADYREGISTRED ":Unauthorized command (already registered)")
(defreply 463 ERR-NOPERMFORHOST ":Your host isn't among the privileged")
(defreply 464 ERR-PASSWDMISMATCH ":Password incorrect")
(defreply 465 ERR-YOUREBANNEDCREEP ":You are banned from this server")
(defreply 466 ERR-YOUWILLBEBANNED)
(defreply 467 ERR-KEYSET "<channel> :Channel key already set")
(defreply 471 ERR-CHANNELISFULL "<channel> :Cannot join channel (+l)")
(defreply 472 ERR-UNKNOWNMODE "<char> :is unknown mode char to me for <channel>")
(defreply 473 ERR-INVITEONLYCHAN "<channel> :Cannot join channel (+i)")
(defreply 474 ERR-BANNEDFROMCHAN "<channel> :Cannot join channel (+b)")
(defreply 475 ERR-BADCHANNELKEY "<channel> :Cannot join channel (+k)")
(defreply 476 ERR-BADCHANMASK "<channel> :Bad Channel Mask")
(defreply 477 ERR-NOCHANMODES "<channel> :Channel doesn't support modes")
(defreply 478 ERR-BANLISTFULL "<channel> <char> :Channel list is full")
(defreply 481 ERR-NOPRIVILEGES ":Permission Denied- You're not an IRC operator")
(defreply 482 ERR-CHANOPRIVSNEEDED "<channel> :You're not channel operator")
(defreply 483 ERR-CANTKILLSERVER ":You can't kill a server!")
(defreply 484 ERR-RESTRICTED ":Your connection is restricted!")
(defreply 485 ERR-UNIQOPPRIVSNEEDED ":You're not the original channel operator")
(defreply 491 ERR-NOOPERHOST ":No O-lines for your host")
(defreply 501 ERR-UMODEUNKNOWNFLAG ":Unknown MODE flag")
