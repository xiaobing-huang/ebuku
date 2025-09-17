;;; ebuku-tests.el --- Tests for ebuku -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Tests for ebuku bookmark management functions, specifically the
;; non-interactive bookmark addition functionality.

;;; Code:

(require 'ert)
(require 'ebuku)

(defvar ebuku-test-temp-db nil
  "Temporary database file for testing.")

(defun ebuku-test-setup ()
  "Set up test environment with temporary database."
  (setq ebuku-test-temp-db (make-temp-file "ebuku-test-" nil ".db"))
  (setq ebuku-database-path ebuku-test-temp-db)
  (setq ebuku-buku-path "buku")
  ;; Disable metadata retrieval for predictable testing
  (setq ebuku-retrieve-url-metadata nil))

(defun ebuku-test-teardown ()
  "Clean up test environment."
  (when (and ebuku-test-temp-db (file-exists-p ebuku-test-temp-db))
    (delete-file ebuku-test-temp-db))
  (setq ebuku-test-temp-db nil))

;; (ert-deftest ebuku-add-bookmark-programmatic-basic ()
;;   "Test basic non-interactive bookmark addition."
;;   (ebuku-test-setup)
;;   (unwind-protect
;;       (let ((result (ebuku-add-bookmark-programmatic "https://example.com"
;;                                                      "Example Site"
;;                                                      '("test" "example")
;;                                                      "A test bookmark")))
;;         (should (stringp result))
;;         (should (string-match-p "^[0-9]+$" result)))
;;     (ebuku-test-teardown)))

(provide 'ebuku-tests)

;;; ebuku-tests.el ends here
