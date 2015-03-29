// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// parsing logic for XEP-0220: Server Dialback (http://xmpp.org/extensions/xep-0220.html)
namespace Yaaf.Xmpp.ServerDialback

open Yaaf.Xmpp
open Yaaf.Helper

type RequestValidateInfo =
  { From : JabberId
    To : JabberId
    Key : string }

type RequestValidateResultInfo =
  { From : JabberId
    To : JabberId
    IsValid : bool }

type UnderLayingError =
  | StreamError of Runtime.XmlStreamError
  | StanzaError of XmlStanzas.StanzaErrorConditon
type ServerDialbackError =
  /// The domain given in the 'to' attribute of the request is not hosted on the Receiving Server. Nonetheless, the domain is used in the 'from' attribute of the error packet, for the purpose of identifying the original request.
  | ItemNotFound
  /// The Receiving Server was unable to establish a connection to the Authoritative Server and therefore could not validate the identity of the Initiating Server.
  | RemoteConnectionFailed
  /// The Receiving Server encountered an <item-not-found/> error condition or a <host-unknown/> stream error when attempting to contact the Authoritative Server.
  | RemoteServerNotFound
  /// The Receiving Server encountered a problem with the connection to the Authoritative Server, for example if the Authoritative Server unexpectedly closed the stream without verifying the dialback key.
  | RemoteServerTimeout
  /// The Receiving Server enforces a policy mandating usage of TLS before dialback and the Initiating Server sent the dialback request without using TLS.
  | PolicyViolation
  /// The Receiving Server enforces a policy requiring either a valid PKIX certificate containing the identity of the Sender Domain or some other proof of authorization (e.g., via POSH), and the Initiating Server did not provide proof of authorization.
  | NotAuthorized
  /// The Receiving Server received an "invalid" response when attempting to verify the dialback key with the Authoritative Server.
  | Forbidden
  /// The Receiving Server was unable to establish the asserted identity of the Initiating Server.
  | NotAcceptable
  /// The Receiving Server lacks the resources to add the requested domain pair to the list of connections authorized for this connection. The initiating server should attempt to establish a new TCP connection to the target domain using the process described in RFC 6120 and give up when receiving the same error on the new connection.
  | ResourceConstraint
  /// Error we do not understand
  | UnknownError of string * string
  member x.UnderlayingError =
    match x with
    | ItemNotFound -> StanzaError XmlStanzas.StanzaErrorConditon.ItemNotFound 
    | RemoteConnectionFailed -> StreamError Runtime.XmlStreamError.RemoteConnectionFailed 
    | RemoteServerNotFound -> StanzaError XmlStanzas.StanzaErrorConditon.RemoteServerNotFound 
    | RemoteServerTimeout -> StanzaError XmlStanzas.StanzaErrorConditon.RemoteServerTimeout
    | PolicyViolation -> StreamError Runtime.XmlStreamError.PolicyViolation
    | NotAuthorized -> StreamError Runtime.XmlStreamError.NotAuthorized
    | Forbidden -> StanzaError XmlStanzas.StanzaErrorConditon.Forbidden 
    | NotAcceptable -> StanzaError XmlStanzas.StanzaErrorConditon.NotAcceptable
    | ResourceConstraint -> StreamError Runtime.XmlStreamError.ResourceConstraint
    | UnknownError (ns, s) -> 
      if ns = XmlStanzas.Parsing.stanzasNs then
        StanzaError (XmlStanzas.StanzaErrorConditon.UnknownCondition s)
      elif ns = Runtime.StreamError.xmppStreamsNS then
        StreamError (Runtime.XmlStreamError.UnknownError s)
      else failwithf "expected stanza or stream namespace for unknown dialback error condition, instead of '%s'" ns
  member x.XmlString =
    match x.UnderlayingError with
    | StanzaError e -> e.XmlString
    | StreamError e -> e.XmlString
  member x.Namespace =
    match x.UnderlayingError with
    | StanzaError _ -> XmlStanzas.Parsing.stanzasNs
    | StreamError _ -> Runtime.StreamError.xmppStreamsNS 
  static member Parse (ns,s) =
    match ns with
    | Equals XmlStanzas.Parsing.stanzasNs ->
      match XmlStanzas.StanzaErrorConditon.Parse s with
      | XmlStanzas.StanzaErrorConditon.ItemNotFound -> ItemNotFound
      | XmlStanzas.StanzaErrorConditon.RemoteServerNotFound -> RemoteServerNotFound
      | XmlStanzas.StanzaErrorConditon.RemoteServerTimeout-> RemoteServerTimeout
      | XmlStanzas.StanzaErrorConditon.Forbidden -> Forbidden
      | XmlStanzas.StanzaErrorConditon.NotAcceptable -> NotAcceptable
      | _ -> UnknownError (ns, s)
    | Equals Runtime.StreamError.xmppStreamsNS ->
      match Runtime.XmlStreamError.Parse s with
      | Runtime.XmlStreamError.RemoteConnectionFailed -> RemoteConnectionFailed
      | Runtime.XmlStreamError.PolicyViolation -> PolicyViolation
      | Runtime.XmlStreamError.NotAuthorized -> NotAuthorized
      | Runtime.XmlStreamError.ResourceConstraint-> ResourceConstraint
      | _ -> UnknownError (ns, s)
    | _ ->
      failwithf "expected stanza or stream namespace for unknown dialback error condition, instead of '%s'" ns 

type RequestValidateErrorInfo =
  { From : JabberId
    To : JabberId
    Error : ServerDialbackError }
  
type DialbackContent =
    | Validate of RequestValidateInfo
    | ValidateResult of RequestValidateResultInfo
    | ValidateError of RequestValidateErrorInfo 
    member x.From =
      match x with
      | Validate c -> c.From
      | ValidateResult c -> c.From
      | ValidateError c -> c.From
    member x.To =
      match x with
      | Validate c -> c.To
      | ValidateResult c -> c.To
      | ValidateError c -> c.To

type ServerDialbackAction =
    | Request of DialbackContent
    /// The id as string and the dialback content
    | DoValidate of string * DialbackContent

module Parsing = 
    open System.Xml.Linq
    open Yaaf.Xml
    
    let serverDialbackNs = "jabber:server:dialback"
    
    open Yaaf.Helper
    
    let isDialbackRequestElement (elem : StreamElement) =
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | Equals serverDialbackNs, "result"  -> true
        | _ -> false
        
    let isDialbackDoValidateElement (elem : StreamElement) =
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | Equals serverDialbackNs, "verify"  -> true
        | _ -> false

    let isDialbackElement (elem : StreamElement) =
        isDialbackRequestElement elem || isDialbackDoValidateElement elem 
    
    let parseDialbackError (elem : XElement) =
        let first = elem.Elements() |> Seq.head
        ServerDialbackError.Parse(first.Name.NamespaceName, first.Name.LocalName)
    
    let parseDialbackContent (elem: StreamElement) =
        let fromAttr = elem |> tryXAttrValue (getXName "from" "")
        let fromValue = JabberId.Parse fromAttr.Value
        let toAttr = elem |> tryXAttrValue (getXName "to" "")
        let toValue = JabberId.Parse toAttr.Value

        let idAttr = elem |> tryXAttrValue (getXName "id" "")
        let typeAttr = elem |> tryXAttrValue (getXName "type" "")
        let content = elem.Value
        match typeAttr with
        | Some "valid"
        | Some "invalid" ->
          idAttr, ValidateResult { From = fromValue; To = toValue; IsValid = typeAttr.Value = "valid" }
        | Some "error" ->
          let errorElem = elem.Element(getXName "error" KnownStreamNamespaces.serverNS)
          if errorElem = null then failwithf "could not find a properly namespaced error element in %A" elem
          idAttr, ValidateError { From = fromValue; To = toValue; Error = parseDialbackError errorElem }
        | None ->
          idAttr, Validate { From = fromValue; To = toValue; Key = content.Trim() }
        | _ -> failwithf "Server Dialback element should have a type of 'valid', 'invalid', 'error' or no type, but not '%s'" typeAttr.Value

    let parseDialbackElement (elem : StreamElement) =
        if elem.Name.LocalName <> "verify" && elem.Name.LocalName <> "result" then
            failwithf "expected 'verify' or 'result' instead of %s" elem.Name.LocalName
        if elem.Name.NamespaceName <> serverDialbackNs then
            failwithf "expected dialback element to have the namespace '%s' instead of '%s'" serverDialbackNs elem.Name.NamespaceName

        let maybeId, content = parseDialbackContent elem 
        match elem.Name.LocalName with
        | "verify" ->
          match maybeId with
          | Some id -> DoValidate (id, content)
          | None -> failwith "expect verify server dialback element to have an ID!"
        | "result" -> Request(content)
        | _ -> 
            failwithf "expected 'verify' or 'result' instead of %s" elem.Name.LocalName

    
    let createDialbackElement (action:ServerDialbackAction) =
        let createErrorElem (e : RequestValidateErrorInfo) =
          [
            yield new XAttribute(getXName "type" "", "cancel") :> obj
            yield getXElem (getXName e.Error.XmlString e.Error.Namespace) :> obj
          ]
          |> getXElemWithChilds (getXName "error" KnownStreamNamespaces.serverNS)
        let createDialbackInner name (maybeId:string option) (content:DialbackContent) =
          [
            yield new XAttribute(getXName "from" "", content.From.FullId) :> obj
            yield new XAttribute(getXName "to" "", content.To.FullId) :> obj
            if maybeId.IsSome then
              yield new XAttribute(getXName "id" "", maybeId.Value) :> obj
              
            match content with
            | Validate v ->
              yield v.Key :> obj
            | ValidateResult c ->
              yield new XAttribute(getXName "type" "", if c.IsValid then "valid" else "invalid" ) :> obj
            | ValidateError c -> 
              yield new XAttribute(getXName "type" "", "error") :> obj
              yield createErrorElem c :> obj 
          ]
          |> getXElemWithChilds (getXName name serverDialbackNs)
        match action with
        | DoValidate (id, content) ->
          createDialbackInner "verify" (Some id) content
        | Request content ->
          createDialbackInner "result" None content 
