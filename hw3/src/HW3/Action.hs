module HW3.Action where

data HiPermission =
    AllowRead
  | AllowWrite

data PermissionException =
  PermissionRequired HiPermission

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }