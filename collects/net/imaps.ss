
(define-signature mzlib:imap^
  (imap-port-number

   imap-connect
   imap-disconnect
   imap-force-disconnect
   imap-reselect
   imap-status

   imap-get-messages
   imap-copy
   imap-store imap-flag->symbol symbol->imap-flag
   imap-expunge
   
   imap-mailbox-exists?
   imap-create-mailbox

   imap-list-child-mailboxes
   imap-get-hierarchy-delimiter))
