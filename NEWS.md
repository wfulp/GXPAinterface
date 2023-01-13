# GXPAinterface 0.2.1

Making `server_url` a parameter for many functions and setting default to BMS server: 

   `server_url = "https://geneatlas.redda.bms.com/"`

# GXPAinterface 0.2.0

New functions for exporting files to GXPA:

   - `begin_new_session()` to create a session (staging area) on GXPA
   - `send_file_to_session()` to export a file to GXPA
   - `login_and_get_user_cookie()` to obtain user cookie needed to export to GXPA
   - `dry_run_session()` after you load files, you can do a dry run to make sure files are formatted properly (quick or full dry run options).
   -  `load_session()`  after your load files, you can run this to trigger the task to load data from the session to the SQL database.
   -  `remove_session()` if you need to remove a series or session, respectively.

# GXPAinterface 0.1.0

* First version
