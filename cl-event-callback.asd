(defsystem #:cl-event-callback
    :depends-on (#:alexandria #:bordeaux-threads)
    :components ((:file "event_thread")))
