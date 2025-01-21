import os
import sequtils
import bitops
import lists
import macros
import json
import tables
import strutils

export keepItIf
export lists

when defined(Windows):
  const STEAM_API = "libsteam_api64.dll"
elif defined(MacOSX):
  const STEAM_API = "libsteam_api.dynlib"
else:
  const STEAM_API = "libsteam_api64.so"

var steamworksInit*: bool

# interfaces
template generateInterface[T](call: untyped, name: string) =
  proc steamImpl(): T {.gensym, stdcall, dynlib: STEAM_API, importc: name.}

  template call*: T =
    if not steamworksInit:
      initSteamworks()
    steamImpl()

# callback system
type
  APIProc* = proc(failed: bool, data: pointer) {.stdcall.}
  APICallHandle* = uint64
  APICallback* = object
    p*: APIProc
    callbackId*: uint32
    callId*: APICallHandle
    finish*: bool
    dataSize*: int

var callbacks* = initDoublyLinkedList[APICallback]()

template registerCallback*[T](
  id: static uint32,
  callback: proc(failed: bool, data: ptr T) {.stdcall.},
) =
  callbacks &= APICallback(
    p: cast[APIProc](callback),
    callbackId: id,
    callId: 0,
    dataSize: sizeof(T),
    finish: false,
  )

# actually generate the api
proc generateSteamAPIImpl*(jsonNode: JsonNode): NimNode =
  result = newStmtList()

  let
    steamIdNode = ident("SteamId")
    gameIdNode = ident("GameId")

  result.add quote do:
    # predefine for order
    proc initSteamworks()

    # for nonflat -> flat
    type
      `steamIdNode`* = distinct uint64
      `gameIdNode`* = distinct uint64

  var
    types = [
      ("bool", ident("bool")),
      ("size_t", ident("csizet")),
      ("intptr_t", ident("int64")),
      ("float", ident("float32")),
      ("double", ident("float64")),
      ("uint8", ident("uint8")),
      ("uint16", ident("uint16")),
      ("uint32", ident("uint32")),
      ("uint64", ident("uint64")),
      ("int8", ident("int8")),
      ("int16", ident("int16")),
      ("int32", ident("int32")),
      ("int64", ident("int64")),
      ("char", ident("char")),
      ("unsigned char", ident("uint8")),
      ("signed char", ident("int8")),
      ("short", ident("int16")),
      ("unsigned short", ident("uint16")),
      ("int", ident("int32")),
      ("unsigned int", ident("uint32")),
      ("long long", ident("int64")),
      ("unsigned long long", ident("uint64")),
      ("char [1024]", quote do:
        array[1024, char],
      ),
      ("CSteamID", steamIdNode),
      ("CGameID", gameIdNode),
      (
        "SteamAPIWarningMessageHook_t",
        quote do:
          pointer,
      ),
    ].toTable()

  for t in types.keys():
    echo "typedef: ", t

  var steamworksInitString = ""

  proc getTypeFromTable(inName: string): NimNode =
    let name = inName
      .strip()

    if name == "char *":
      quote do: cstring
    elif name == "void":
      nil
    elif name.startsWith("const "):
      getTypeFromTable(name[5..^1])
    elif name.endsWith("]"):
      let
        idx = name.rfind("[")
        countStr = name[idx + 1..^2]
        count = parseInt(countStr)

      let tmp = getTypeFromTable(name[0..idx - 1])

      quote do:
        array[`count`, `tmp`]

    elif name.endsWith("*") or name.endsWith("&"):
      let tmp = getTypeFromTable(name[0..^2])
      if tmp == nil or tmp.kind == nnkTupleTy:
        quote do:
          pointer
      else:
        quote do:
          ptr `tmp`
    elif name in types:
      types[name]
    elif name.endsWith(")"):
      quote do:
        pointer
    elif "::" in name:
      # normally a member enum
      quote do:
        int32
    else:
      echo "missing type: " & inName
      quote do:
        tuple[]

  for td in jsonNode["typedefs"]:
    let
      rawName = td["typedef"].getStr()
      rawTo = td["type"].getStr()

    if rawName in types:
      continue

    let identTo = getTypeFromTable(rawTo)
    if identTo == nil:
      continue

    let
      identName = ident(rawName.replace("_t", ""))

    echo "typedef: " & $identName & ", " & rawTo

    result.add quote do:
      type `identName`* = distinct `identTo`

    types[rawName] = identName

  type
    CallbackEntry = object
      typeName: NimNode
      id: int

  for e in jsonNode["enums"]:
    var fieldNames: seq[string]
    var fieldValues: seq[NimNode]
    for value in e["values"]:
      let
        name = value["name"]
          .getStr()
          .replace("k_", "")
          .replace("K_", "")
          .strip(chars = {'_'})
        val = newLit(value["value"].getStr())

      fieldNames &= name
      fieldValues &= val

    var matchLen = fieldNames[0].high

    for f in 1..<fieldNames.len:
      matchLen = min(fieldNames[f].high, matchLen)
      while matchLen > 0 and
            fieldNames[0][0..matchLen] != fieldNames[f][0..matchLen]:
        matchLen -= 1

    let
      name = ident(
        e["enumname"].getStr()
      )

    types[$name] = name

    echo "enum: " & $name

    var fields: seq[NimNode]

    for f in 0..<fieldNames.len:
      let
        nimName = nimIdentNormalize(
          fieldNames[f][matchLen + 1].toLowerAscii() &
          fieldNames[f][matchLen + 2..^1]
        )
        steamName = nimIdentNormalize(
          fieldNames[f][0].toLowerAscii() &
          fieldNames[f][1..^1]
        )
      fields.add(newNimNode(nnkEnumFieldDef)
        .add(ident(steamName))
        .add(fieldValues[f])
      )
      echo "  value: " & steamName & " = " & $fieldValues[f]
      if steamName != nimName and nimName.len > 3:
        fields.add(newNimNode(nnkEnumFieldDef)
          .add(ident(nimName))
          .add(fieldValues[f])
        )
        echo "  value: " & nimName & " = " & $fieldValues[f]

    result.add newEnum(
      name = name,
      fields = fields,
      public = true, pure = true,
    )

  for s in jsonNode["structs"]:
    let
      name = s["struct"].getStr()
      nameNode = ident(
        name.replace("_t", "")
      )

    types[name] = nameNode

    echo "struct: " & name

    var records = if s["fields"].len == 0: newEmptyNode()
                  else: newNimNode(nnkRecList)

    for f in s["fields"]:
      var
        inName = f["fieldname"].getStr().replace("m_", "")
        name = if inName.len > 1 and
                    (
                      inName[0..0] == "b" or
                      inName[0..0] == "c" or
                      inName[0..0] == "n" or
                      inName[0..0] == "h" or
                      inName[0..0] == "e"
                    ) and
                  inName[1].isUpperAscii():
                 inName[1..^1]
               elif inName.len > 3 and
                    (
                      inName[0..2] == "pch"
                    ) and
                    inName[3].isUpperAscii():
                 inName[3..^1]
               else:
                 inName
        fixedName = name[0].toLowerAscii() & name[1..^1]
        cleanedName = fixedName
          .replace("ID", "Id")
          .replace("_", "")
        typeName = f["fieldtype"].getStr()
        typeIdent = getTypeFromTable(typeName)

      echo "  field: " & cleanedName & ", " & typeName

      records.add(
        newIdentDefs(
          name = postfix(ident(cleanedName), "*"),
          kind = typeIdent,
        )
      )

    let typeBody = newNimNode(nnkObjectTy)
          .add(newEmptyNode())
          .add(newEmptyNode())
          .add(records)

    result.add quote do:
      type `nameNode`* = `typeBody`

  var callbackData: Table[string, CallbackEntry]
  for e in jsonNode["callback_structs"]:
    let
      name = e["struct"].getStr()
      id = e["callback_id"].getInt()
      outName = ident(
        name.replace("_t", "")
      )

      handleName = name & "Handle"
      outHandleName = ident(handleName.replace("_t", ""))

    echo "callback[" & $id & "]: " & $name

    types[name] = outName
    types[handleName] = outHandleName

    callbackData[name] = CallbackEntry(
      typeName: outName,
      id: id,
    )

    var records = if e["fields"].len == 0: newEmptyNode()
                  else: newNimNode(nnkRecList)

    for f in e["fields"]:
      var
        inName = f["fieldname"].getStr().replace("m_", "")
        name = if inName.len > 1 and
                    (
                      inName[0..0] == "b" or
                      inName[0..0] == "c" or
                      inName[0..0] == "n" or
                      inName[0..0] == "h" or
                      inName[0..0] == "e"
                    ) and
                  inName[1].isUpperAscii():
                 inName[1..^1]
               elif inName.len > 3 and
                    (
                      inName[0..2] == "pch"
                    ) and
                    inName[3].isUpperAscii():
                 inName[3..^1]
               else:
                 inName
        fixedName = name[0].toLowerAscii() & name[1..^1]
        cleanedName = fixedName
          .replace("ID", "Id")
          .replace("_", "")
        typeName = f["fieldtype"].getStr()
        typeIdent = getTypeFromTable(typeName)

      echo "  field: " & cleanedName & ", " & typeName

      records.add(
        newIdentDefs(
          name = postfix(ident(cleanedName), "*"),
          kind = typeIdent,
        )
      )

    let typeBody = newNimNode(nnkObjectTy)
          .add(newEmptyNode())
          .add(newEmptyNode())
          .add(records)

    result.add quote do:
      type
        `outName`* = `typeBody`
        `outHandleName`* = distinct APICallHandle

      template onDone*(
        r: `outHandleName`,
        callback: proc(failed: bool, data: ptr `outName`) {.stdcall.},
      ) =
        callbacks &= APICallback(
          p: cast[proc(failed: bool, data: pointer) {.stdcall.}](callback),
          callbackId: `id`.uint32,
          callId: r.APICallHandle,
          dataSize: sizeof(`outName`),
          finish: true,
        )

  for i in jsonNode["interfaces"]:
    let
      name = i["classname"].getStr()
      nameNode = ident(name)

    types[name] = nameNode

    echo "interface: " & name

    if "version_string" in i:
      steamworksInitString &= i["version_string"].getStr()
      steamworksInitString &= "\x00"

    result.add quote do:
      type `nameNode`* = distinct pointer

    if "accessors" in i:
      for a in i["accessors"][0..0]:
        let
          bindName = ident(
            a["name"].getStr()
              .replace("SteamAPI", "")
              .replace("Steam", "steam")
              .replace("_", "")
          )
          nameFlat = a["name_flat"].getStr()

        echo "  accessor: " & $bindname

        result.add quote do:
          `generateInterface`[`nameNode`](`bindName`, `nameFlat`)

    if "methods" in i:
      for m in i["methods"]:
        let
          methodName = m["methodname"].getStr()
          methodNameFlat = m["methodname_flat"].getStr()

          procName = ident(methodName[0..0].toLowerAscii() & methodName[1..^1])

        echo "  method: " & $procName & ", " & $methodNameFlat
        var procNode = quote do:
          proc `procName`*(self: `nameNode`) {.stdcall, dynlib: `STEAM_API`, importc: `methodNameFlat`}

        for p in m["params"]:
          let
            paramName = p["paramname"].getStr()
                        .replace("_", "")
                        .toLowerAscii()
            typeName = p["paramtype"].getStr()
            typeIdent = getTypeFromTable(typeName)

          echo "    param: " & paramName & ", " & typeName

          procNode.params.add newIdentDefs(
            name = ident(paramName),
            kind = typeIdent,
          )

        if "callresult" in m:
          let
            callback = m["callresult"].getStr()
            handleIdent = getTypeFromTable(callback & "Handle")
            outType = callbackData[callback].typeName
            id = callbackData[callback].id

          procNode.params[0] = handleIdent
          echo "    callback[" & $id & "]: " & $outType
        else:
          let
            typeName = m["returntype"].getStr()
          if typeName != "void":
            procNode.params[0] = getTypeFromTable(typeName)
            echo "    return: " & typeName

        result.add procNode

  steamworksInitString &= "\x00"

  let initString = newStrLitNode(steamworksInitString)

  result.add quote do:
    type
      CallbackMsg {.pure, bycopy.} = object
        user: HSteamUser
        callback: uint32
        param: pointer
        paramSize: uint32

    # init stuff
    proc initInternal(versions: cstring, msg: pointer): int32 {.stdcall, dynlib: STEAM_API, importc: "SteamInternal_SteamAPI_Init".}

    export lists

    proc initSteamworks() =
      var msg: array[1024, char]
      if initInternal(`initString`.cstring, addr msg[0]) != 0:
        let res = $cast[cstring](addr msg[0])
        raise newException(OSError, "steamworks init fail" & $res)

      steamworksInit = true

    type
      PSteamPipe = ptr HSteamPipe

    # some utils not in json
    proc restartAppIfNecessary*(ownAppID: AppId): bool {.stdcall, dynlib: STEAM_API, importc: "SteamAPI_RestartAppIfNecessary".}
    export registerCallback

    # for manual callbacks
    proc initManualDispatch*() {.stdcall, dynlib: STEAM_API, importc: "SteamAPI_ManualDispatch_Init".}
    proc runManualDispatchFrame(pipe: PSteamPipe) {.stdcall, dynlib: STEAM_API, importc: "SteamAPI_ManualDispatch_RunFrame"}
    proc nextManualDispatchCallback(
      pipe: PSteamPipe, callback: ptr CallbackMsg,
    ): bool {.stdcall, dynlib: STEAM_API, importc: "SteamAPI_ManualDispatch_GetNextCallback".}
    proc freeLastManualDispatchCallback(pipe: PSteamPipe) {.stdcall, dynlib: STEAM_API, importc: "SteamAPI_ManualDispatch_FreeLastCallback".}
    proc getCallbackResult(
      pipe: PSteamPipe, call: SteamAPICall, callback: pointer,
      size: uint32, expected: int32, failed: ptr bool,
    ): bool {.stdcall, dynlib: STEAM_API, importc: "SteamAPI_ManualDispatch_GetAPICallResult".}

    `generateInterface`[PSteamPipe](steamPipe, "SteamAPI_GetHSteamPipe")

    var manualInit: bool

    proc runFrame*(steamUtils: ISteamUtils) =
      if not steamworksInit:
        initSteamworks()

      if not manualInit:
        initManualDispatch()
        manualInit = true

      steamPipe().runManualDispatchFrame()

      var cbm: CallbackMsg
      while steamPipe().nextManualDispatchCallback(addr cbm):
        if cbm.callback == 703:
          let completed = cast[ptr SteamAPICallCompleted](cbm.param)[]
          let data = allocShared(completed.cubParam)
          let failed = true

          if steamPipe().getCallbackResult(
            completed.asyncCall, data, completed.cubParam,
            completed.iCallback, addr failed,
          ):
            var cn = callbacks.head
            while cn != nil:
              let c = cn.value

              if c.callId.uint64 == completed.asyncCall.uint64:
                c.p(failed, data)

                if c.finish:
                  callbacks.remove(cn)
              cn = cn.next
          deallocShared(data)
        else:
          var cn = callbacks.head
          while cn != nil:
            let c = cn.value

            if c.callbackId == cbm.callback:
              var data: pointer = addr cbm.param
              var fail: bool = false
              c.p(fail, data)

              if c.finish:
                callbacks.remove(cn)
            cn = cn.next

        steamPipe().freeLastManualDispatchCallback()

macro generateSteamAPI*(json: static string): untyped =
  let
    jsonRaw = staticRead(json)
    jsonNode = parseJson(jsonRaw)

  generateSteamAPIImpl(jsonNode)
