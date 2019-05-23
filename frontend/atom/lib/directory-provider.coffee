module.exports =
class RemoteDirectoryProvider
    directoryForURISync: () ->
        console.log("REQUESTED DIR!")
        new RemoteDirectory
    directoryForURI: () ->
        Promise.resolve(@directoryForURISync())


class RemoteDirectory
    onDidChange: () ->
    isFile: () -> false
    isDirectory: () -> true
    isSymbolicLink: () -> false
    exists: -> Promise.resolve(true)
    existsSync: -> true
    isRoot: -> true
    getPath: -> "/ROOT"
    getRealPathSync: -> "/ROOT"
    getBaseName: -> "ROOT"
    relativize: (a) -> a
    getParent: () -> @
    getSubdirectory: () -> @
    getFile: () -> @
    getEntriesSync: () -> [@]
    getEntries: (callback) -> callback(null, [@,@])
    contains: () -> false
