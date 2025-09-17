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

(ert-deftest ebuku-add-bookmark-programmatic-basic ()
  "Test basic non-interactive bookmark addition."
  (ebuku-test-setup)
  (unwind-protect
      (let ((result (ebuku-add-bookmark-programmatic "https://example.com"
                                                     "Example Site"
                                                     '("test" "example")
                                                     "A test bookmark")))
        (should (stringp result))
        (should (string-match-p "^[0-9]+$" result)))
    (ebuku-test-teardown)))

(ert-deftest ebuku-add-bookmark-programmatic-minimal ()
  "Test adding bookmark with only URL."
  (ebuku-test-setup)
  (unwind-protect
      (let ((result (ebuku-add-bookmark-programmatic "https://minimal.com")))
        (should (stringp result))
        (should (string-match-p "^[0-9]+$" result)))
    (ebuku-test-teardown)))

(ert-deftest ebuku-add-bookmark-programmatic-duplicate ()
  "Test handling of duplicate bookmarks."
  (ebuku-test-setup)
  (unwind-protect
      (progn
        ;; Add initial bookmark
        (ebuku-add-bookmark-programmatic "https://duplicate.com" "Original")
        ;; Try to add duplicate - should signal error
        (should-error (ebuku-add-bookmark-programmatic "https://duplicate.com" "Duplicate")
                      :type 'user-error))
    (ebuku-test-teardown)))

(ert-deftest ebuku-add-bookmark-programmatic-invalid-url ()
  "Test handling of invalid URLs."
  (ebuku-test-setup)
  (unwind-protect
      ;; buku might accept URLs that seem invalid, so we test with nil URL
      (should-error (ebuku-add-bookmark-programmatic nil)
                    :type 'error)
    (ebuku-test-teardown)))

(ert-deftest ebuku-add-bookmark-programmatic-with-tags ()
  "Test adding bookmark with various tag formats."
  (ebuku-test-setup)
  (unwind-protect
      (progn
        ;; Test with list of tags
        (let ((result1 (ebuku-add-bookmark-programmatic "https://tags1.com"
                                                       "Tags Test 1"
                                                       '("tag1" "tag2" "tag3"))))
          (should (stringp result1)))
        ;; Test with empty tags
        (let ((result2 (ebuku-add-bookmark-programmatic "https://tags2.com"
                                                       "Tags Test 2"
                                                       '())))
          (should (stringp result2)))
        ;; Test with nil tags
        (let ((result3 (ebuku-add-bookmark-programmatic "https://tags3.com"
                                                       "Tags Test 3"
                                                       nil)))
          (should (stringp result3))))
    (ebuku-test-teardown)))

(provide 'ebuku-tests)

;;; ebuku-tests.el ends here
