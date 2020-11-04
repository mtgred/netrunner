# Jnet backups/restores

The `backup` and `restore` scripts make local backups of the jinteki.net mongodb database.

In order to function, these scripts require mongodump and mongorestore to be installed and available in `PATH`.

## Backup

In order to backup the database, run

    BACKUPS_KEEP_LAST=5 ./backups/backup

This will back up the mongo database to
`./backups/data/netrunner-$(date).gzip`, then delete oldest backups until 5
backups remain.

In order to backup to a different directory, specify `BACKUP_DIR`. e.g.

    BACKUP_DIR=/mnt/backups BACKUPS_KEEP_LAST=5 ./backups/backup

If you provide a relative path, it will be relative to the `./backups/`
directory in this repo.

## Restore

In order to restore a backup, run

    ./backups/restore /path/to/backup

e.g.

    ./backups/restore backups/data/netrunner-2020-08-15T13:17:54+01:00.gzip
