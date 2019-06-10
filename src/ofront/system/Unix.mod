MODULE Unix;  (* Josef Templ, 5.3.90  Linux system calls *)

(* Module Unix provides a system call interface to Linux.
  Naming conventions:
    Procedure and Type-names always start with a capital letter.
    error numbers as defined in Unix
    other constants start with lower case letters *)

IMPORT SYSTEM;

CONST

(* various important constants *)

  stdin* = 0; stdout* =1; stderr* = 2;

  LOCKEX* = 2; LOCKUN* = 8;  (* /usr/include/file.h *)
  AFINET* = 2; (* /usr/include/sys/socket.h *)
  PFINET* = AFINET; (* /usr/include/linux/socket.h *)
  SOCKSTREAM* = 1; (* /usr/include/linux/socket.h *)
  FIONREAD* =  541BH; (* in /usr/include/asm/termios.h *)
  SETFL* = 4; (* set file descriptor flags; in asm/fcntl.h *)
  TCP* = 0;

(* flag sets, cf. /usr/include/asm/fcntl.h *)
  rdonly* = {}; wronly* = {0}; rdwr* = {1}; creat* = {6}; excl* = {7}; trunc* = {9}; append* = {10}; ndelay = {11};

(* error numbers *)

  EPERM* = 1;  (* Not owner *)
  ENOENT* = 2;  (* No such file or directory *)
  ESRCH* = 3;  (* No such process *)
  EINTR* = 4;  (* Interrupted system call *)
  EIO* = 5;  (* I/O error *)
  ENXIO* = 6;  (* No such device or address *)
  E2BIG* = 7;  (* Arg list too long *)
  ENOEXEC* = 8;  (* Exec format error *)
  EBADF* = 9;  (* Bad file number *)
  ECHILD* = 10;  (* No children *)
  EAGAIN* = 11;  (* No more processes *)
  ENOMEM* = 12;  (* Not enough core *)
  EACCES* = 13;  (* Permission denied *)
  EFAULT* = 14;  (* Bad address *)
  ENOTBLK* = 15;  (* Block device required *)
  EBUSY* = 16;  (* Mount device busy *)
  EEXIST* = 17;  (* File exists *)
  EXDEV* = 18;  (* Cross-device link *)
  ENODEV* = 19;  (* No such device *)
  ENOTDIR* = 20;  (* Not a directory*)
  EISDIR* = 21;  (* Is a directory *)
  EINVAL* = 22;  (* Invalid argument *)
  ENFILE* = 23;  (* File table overflow *)
  EMFILE* = 24;  (* Too many open files *)
  ENOTTY* = 25;  (* Not a typewriter *)
  ETXTBSY* = 26;  (* Text file busy *)
  EFBIG* = 27;  (* File too large *)
  ENOSPC* = 28;  (* No space left on device *)
  ESPIPE* = 29;  (* Illegal seek *)
  EROFS* = 30;  (* Read-only file system *)
  EMLINK* = 31;  (* Too many links *)
  EPIPE* = 32;  (* Broken pipe *)
  EDOM* = 33;  (* Argument too large *)
  ERANGE* = 34;  (* Result too large *)
  EDEADLK* = 35;  (* Resource deadlock would occur *)
  ENAMETOOLONG* = 36;  (* File name too long *)
  ENOLCK* = 37;  (* No record locks available *)
  ENOSYS* = 38;  (* Function not implemented *)
  ENOTEMPTY* = 39;  (* Directory not empty *)
  ELOOP* = 40;  (* Too many symbolic links encountered *)
  EWOULDBLOCK* = EAGAIN;  (* Operation would block *)
  ENOMSG* = 42;  (* No message of desired type *)
  EIDRM* = 43;  (* Identifier removed *)
  ECHRNG* = 44;  (* Channel number out of range *)
  EL2NSYNC* = 45;  (* Level 2 not synchronized *)
  EL3HLT* = 46;  (* Level 3 halted *)
  EL3RST* = 47;  (* Level 3 reset *)
  ELNRNG* = 48;  (* Link number out of range *)
  EUNATCH* = 49;  (* Protocol driver not attached *)
  ENOCSI* = 50;  (* No CSI structure available *)
  EL2HLT* = 51;  (* Level 2 halted *)
  EBADE* = 52;  (* Invalid exchange *)
  EBADR* = 53;  (* Invalid request descriptor *)
  EXFULL* = 54;  (* Exchange full *)
  ENOANO* = 55;  (* No anode *)
  EBADRQC* = 56;  (* Invalid request code *)
  EBADSLT* = 57;  (* Invalid slot *)
  EDEADLOCK* = 58;  (* File locking deadlock error *)
  EBFONT* = 59;  (* Bad font file format *)
  ENOSTR* = 60;  (* Device not a stream *)
  ENODATA* = 61;  (* No data available *)
  ETIME* = 62;  (* Timer expired *)
  ENOSR* = 63;  (* Out of streams resources *)
  ENONET* = 64;  (* Machine is not on the network *)
  ENOPKG* = 65;  (* Package not installed *)
  EREMOTE* = 66;  (* Object is remote *)
  ENOLINK* = 67;  (* Link has been severed *)
  EADV* = 68;  (* Advertise error *)
  ESRMNT* = 69;  (* Srmount error *)
  ECOMM* = 70;  (* Communication error on send *)
  EPROTO* = 71;  (* Protocol error *)
  EMULTIHOP* = 72;  (* Multihop attempted *)
  EDOTDOT* = 73;  (* RFS specific error *)
  EBADMSG* = 74;  (* Not a data message *)
  EOVERFLOW* = 75;  (* Value too large for defined data type *)
  ENOTUNIQ* = 76;  (* Name not unique on network *)
  EBADFD* = 77;  (* File descriptor in bad state *)
  EREMCHG* = 78;  (* Remote address changed *)
  ELIBACC* = 79;  (* Can not access a needed shared library *)
  ELIBBAD* = 80;  (* Accessing a corrupted shared library *)
  ELIBSCN* = 81;  (* .lib section in a.out corrupted *)
  ELIBMAX* = 82;  (* Attempting to link in too many shared libraries *)
  ELIBEXEC* = 83;  (* Cannot exec a shared library directly *)
  EILSEQ* = 84;  (* Illegal byte sequence *)
  ERESTART* = 85;  (* Interrupted system call should be restarted *)
  ESTRPIPE* = 86;  (* Streams pipe error *)
  EUSERS* = 87;  (* Too many users *)
  ENOTSOCK* = 88;  (* Socket operation on non-socket *)
  EDESTADDRREQ* = 89;  (* Destination address required *)
  EMSGSIZE* = 90;  (* Message too long *)
  EPROTOTYPE* = 91;  (* Protocol wrong type for socket *)
  ENOPROTOOPT* = 92;  (* Protocol not available *)
  EPROTONOSUPPORT* = 93;  (* Protocol not supported *)
  ESOCKTNOSUPPORT* = 94;  (* Socket type not supported *)
  EOPNOTSUPP* = 95;  (* Operation not supported on transport endpoint *)
  EPFNOSUPPORT* = 96;  (* Protocol family not supported *)
  EAFNOSUPPORT* = 97;  (* Address family not supported by protocol *)
  EADDRINUSE* = 98;  (* Address already in use *)
  EADDRNOTAVAIL* = 99;  (* Cannot assign requested address *)
  ENETDOWN* = 100;  (* Network is down *)
  ENETUNREACH* = 101;  (* Network is unreachable *)
  ENETRESET* = 102;  (* Network dropped connection because of reset *)
  ECONNABORTED* = 103;  (* Software caused connection abort *)
  ECONNRESET* = 104;  (* Connection reset by peer *)
  ENOBUFS* = 105;  (* No buffer space available *)
  EISCONN* = 106;  (* Transport endpoint is already connected *)
  ENOTCONN* = 107;  (* Transport endpoint is not connected *)
  ESHUTDOWN* = 108;  (* Cannot send after transport endpoint shutdown *)
  ETOOMANYREFS* = 109;  (* Too many references: cannot splice *)
  ETIMEDOUT* = 110;  (* Connection timed out *)
  ECONNREFUSED* = 111;  (* Connection refused *)
  EHOSTDOWN* = 112;  (* Host is down *)
  EHOSTUNREACH* = 113;  (* No route to host *)
  EALREADY* = 114;  (* Operation already in progress *)
  EINPROGRESS* = 115;  (* Operation now in progress *)
  ESTALE* = 116;  (* Stale NFS file handle *)
  EUCLEAN* = 117;  (* Structure needs cleaning *)
  ENOTNAM* = 118;  (* Not a XENIX named type file *)
  ENAVAIL* = 119;  (* No XENIX semaphores available *)
  EISNAM* = 120;  (* Is a named type file *)
  EREMOTEIO* = 121;  (* Remote I/O error *)
  EDQUOT* = 122;  (* Quota exceeded *)


TYPE
  JmpBuf* = RECORD
    bx*, si*, di*, bp*, sp*, pc*: LONGINT;
    maskWasSaved*, savedMask*: LONGINT;
  END ;

  Status* = RECORD (* struct stat *)
    dev*, devX*: LONGINT; (* 64 bit in Linux 2.2 *)
    pad1: INTEGER;
    ino*, mode*, nlink*, uid*, gid*: LONGINT;
    rdev*, rdevX*: LONGINT; (* 64 bit in Linux 2.2 *)
    pad2: INTEGER;
    size*, blksize*, blocks*, atime*, unused1*, mtime*, unused2*, ctime*, 
    unused3*, unused4*, unused5*: LONGINT;
  END ;
  
  Timeval* = RECORD
    sec*, usec*: LONGINT
  END ;

  Timezone* = RECORD
    minuteswest*, dsttime*: LONGINT
  END ;

  Itimerval* = RECORD
    interval*, value*: Timeval
  END ;

  FdSet* = ARRAY 8 OF SET;

  SigCtxPtr* = POINTER TO SigContext;
  SigContext* = RECORD
  END ;

  SignalHandler* = PROCEDURE (sig, code: LONGINT; scp: SigCtxPtr);

  Dirent* = RECORD
    ino, off: LONGINT;
    reclen: INTEGER;
    name: ARRAY 256 OF CHAR;
  END ;

  Rusage* = RECORD
    utime*, stime*: Timeval;
    maxrss*, ixrss*, idrss*, isrss*,
    minflt*, majflt*, nswap*, inblock*,
    oublock*, msgsnd*, msgrcv*, nsignals*,
    nvcsw*, nivcsw*: LONGINT
  END ;

  Iovec* = RECORD
    base*, len*: LONGINT
  END ;

  SocketPair* = ARRAY 2 OF LONGINT;

  Pollfd* = RECORD
    fd*: LONGINT;
    events*, revents*: INTEGER
  END ;

  Sockaddr* = RECORD
    family*: INTEGER;
    port*: INTEGER;
    internetAddr*: LONGINT;
    pad*: ARRAY 8 OF CHAR;
  END ;
  
  HostEntry* = POINTER [1] TO Hostent;
  Hostent* = RECORD
    name*, aliases*: LONGINT;
    addrtype*, length*: LONGINT;
    addrlist*: LONGINT; (*POINTER TO POINTER TO LONGINT, network byte order*)
  END;

  Name* = ARRAY OF CHAR;

  PROCEDURE -includeStat()
    "#include <sys/stat.h>";

  PROCEDURE -includeErrno()
    "#include <errno.h>";

  PROCEDURE -err(): LONGINT
    "errno";

  PROCEDURE errno*(): LONGINT;
  BEGIN
    RETURN err()
  END errno;

  PROCEDURE -Exit*(n: LONGINT)
    "exit(n)";

  PROCEDURE -Fork*(): LONGINT
    "fork()";

  PROCEDURE -Wait*(VAR status: LONGINT): LONGINT
    "wait(status)";

  PROCEDURE -Select*(width: LONGINT; VAR readfds, writefds, exceptfds: FdSet; VAR timeout: Timeval): LONGINT
    "select(width, readfds, writefds, exceptfds, timeout)";

  PROCEDURE -Gettimeofday* (VAR tv: Timeval; VAR tz: Timezone)
    "gettimeofday(tv, tz)";

  PROCEDURE -Read* (fd, buf, nbyte: LONGINT): LONGINT
    "read(fd, buf, nbyte)";

  PROCEDURE -ReadBlk* (fd: LONGINT; VAR buf: ARRAY OF SYSTEM.BYTE): LONGINT
    "read(fd, buf, buf__len)";

  PROCEDURE -Write* (fd, buf, nbyte: LONGINT): LONGINT
    "write(fd, buf, nbyte)";

  PROCEDURE -WriteBlk* (fd: LONGINT; VAR buf: ARRAY OF SYSTEM.BYTE): LONGINT
    "write(fd, buf, buf__len)";

  PROCEDURE -Dup*(fd: LONGINT): LONGINT
    "dup(fd)";

  PROCEDURE -Dup2*(fd1, fd2: LONGINT): LONGINT
    "dup(fd1, fd2)";

  PROCEDURE -Getpid*(): LONGINT
    "getpid()";

  PROCEDURE -Getuid*(): LONGINT
    "getuid()";

  PROCEDURE -Geteuid*(): LONGINT
    "geteuid()";

  PROCEDURE -Getgid*(): LONGINT
    "getgid()";

  PROCEDURE -Getegid*(): LONGINT
    "getegid()";

  PROCEDURE -Unlink*(name: Name): LONGINT
    "unlink(name)";

  PROCEDURE -Open*(name: Name; flag, mode: SET): LONGINT
    "open(name, flag, mode)";

  PROCEDURE -Close*(fd: LONGINT): LONGINT
    "close(fd)";

  PROCEDURE -stat(name: Name; VAR statbuf: Status): LONGINT
    "stat((const char*)name, (struct stat*)statbuf)";

  PROCEDURE Stat*(name: Name; VAR statbuf: Status): LONGINT;
    VAR res: LONGINT;
  BEGIN
    res := stat(name, statbuf);
    (* make the first 4 bytes as unique as possible (used in module Files for caching!) *) 
    INC(statbuf.dev, statbuf.devX);
    INC(statbuf.rdev, statbuf.rdevX);
    RETURN res;
  END Stat;

  PROCEDURE -fstat(fd: LONGINT; VAR statbuf: Status): LONGINT   
    "fstat(fd, (struct stat*)statbuf)";

  PROCEDURE Fstat*(fd: LONGINT; VAR statbuf: Status): LONGINT;
    VAR res: LONGINT;
  BEGIN
    res := fstat(fd, statbuf);
    (* make the first 4 bytes as unique as possible (used in module Files for caching!) *) 
    INC(statbuf.dev, statbuf.devX); 
    INC(statbuf.rdev, statbuf.rdevX);
    RETURN res;
  END Fstat;

  PROCEDURE -Fchmod*(fd, mode: LONGINT): LONGINT
    "fchmod(fd, mode)";

  PROCEDURE -Chmod*(path: Name; mode: LONGINT): LONGINT
    "chmod(path, mode)";

  PROCEDURE -Lseek*(fd, offset, origin: LONGINT): LONGINT
    "lseek(fd, offset, origin)";

  PROCEDURE -Fsync*(fd: LONGINT): LONGINT
    "fsync(fd)";

  PROCEDURE -Fcntl*(fd, cmd, arg: LONGINT ): LONGINT
    "fcntl(fd, cmd, arg)";

  PROCEDURE -Flock*(fd, operation: LONGINT): LONGINT
    "flock(fd, operation)";

  PROCEDURE -Ftruncate*(fd, length: LONGINT): LONGINT
    "ftruncate(fd, length)";

  PROCEDURE -Readblk*(fd: LONGINT; VAR buf: ARRAY OF SYSTEM.BYTE; len: LONGINT): LONGINT
    "read(fd, buf, len)";

  PROCEDURE -Rename*(old, new: Name): LONGINT
    "rename(old, new)";

  PROCEDURE -Chdir*(path: Name): LONGINT
    "chdir(path)";

  PROCEDURE -Ioctl*(fd, request, arg: LONGINT): LONGINT
    "ioctl(fd, request, arg)";

  PROCEDURE -Kill*(pid, sig: LONGINT): LONGINT
    "kill(pid, sig)";

  PROCEDURE -Sigsetmask*(mask: LONGINT): LONGINT
    "sigsetmask(mask)";


  (* TCP/IP networking *)

  PROCEDURE -Gethostbyname*(name: Name): HostEntry
    "(Unix_HostEntry)gethostbyname(name)";

  PROCEDURE -Gethostname*(VAR name: Name): LONGINT
    "gethostname(name, name__len)";

  PROCEDURE -Socket*(af, type, protocol: LONGINT): LONGINT
    "socket(af, type, protocol)";

  PROCEDURE -Connect*(socket: LONGINT; name: Sockaddr; namelen: LONGINT): LONGINT
    "connect(socket, &(name), namelen)";

  PROCEDURE -Getsockname*(socket: LONGINT; VAR name: Sockaddr; VAR namelen: LONGINT): LONGINT
    "getsockname(socket, name, namelen)";

  PROCEDURE -Bind*(socket: LONGINT; name: Sockaddr; namelen: LONGINT): LONGINT
    "bind(socket, &(name), namelen)";

  PROCEDURE -Listen*(socket, backlog: LONGINT): LONGINT
    "listen(socket, backlog)";

  PROCEDURE -Accept*(socket: LONGINT; VAR addr: Sockaddr; VAR addrlen: LONGINT): LONGINT
    "accept(socket, addr, addrlen)";

  PROCEDURE -Recv*(socket, bufadr, buflen, flags: LONGINT): LONGINT
    "recv(socket, bufadr, buflen, flags)";

  PROCEDURE -Send*(socket, bufadr, buflen, flags: LONGINT): LONGINT
    "send(socket, bufadr, buflen, flags)";

END Unix.
