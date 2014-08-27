Summer of LMS 2014
==================

Tracking progress of ongoing LMS Hackathon.

New website
-----------

http://scala-lms.github.io

http://scala-lms.github.io/tutorials/

Virtualization with macros
--------------------------

https://github.com/scala-lms/summer-of-lms-2014/tree/scala-virtualized

https://github.com/tiarkrompf/virtualization-lms-core/tree/macro-trans

Issues:

https://github.com/scala-lms/summer-of-lms-2014/tree/scala-virtualized/ISSUES

### How to help with porting LMS

* Get and install the macro-based Scala-Virtualized
  * `git clone -b scala-virtualized https://github.com/scala-lms/summer-of-lms-2014.git scala-virtualized`
  * `cd scala-virtualized; sbt publish-local`

* Get the `macro-trans` branch of LMS, which contains the work-in-progress port
  * `git clone -b macro-trans git@github.com:TiarkRompf/virtualization-lms-core.git lms-macro-trans`
  * `cd lms-macro-trans`
  * `sbt test`

* Pick a disabled file in the tests. These start with `/*TODO DISABLED`
  * `grep "^/\*TODO DISABLED" test-src/epfl/*/*scala`

* FYI, some source files might also still be disabled. As with test files, they start with `/*TODO DISABLED`
  * `grep "^/\*TODO DISABLED" src/*/*scala`

* Steps to port a disabled file:
  * Uncomment it.
  * `sbt test`
  * Uncomment any other required files.
  * `sbt test`
  * Fix until green.

* Guidelines
  * Only commit and push when `sbt test` is completely green. Travis watches.
  * Use `NOTE(trans)` and `TODO(trans)` in comments to document anything related to the transition.
  * Use `TODO(cleanup)` in comments to document anything that you notice that needs fixing but is unrelated to the transition.
 
