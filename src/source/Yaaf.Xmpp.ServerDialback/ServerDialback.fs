// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// Implementation of XEP-0220: Server Dialback (http://xmpp.org/extensions/xep-0220.html)
namespace Yaaf.Xmpp.ServerDialback


open Yaaf.Logging
open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.XmlStanzas

/// Defines the service to set and request vcards
//type IServerDialbackService =
//    /// Request a vcard from an entity (None for ourself)
//    abstract RequestVCard : JabberId option -> Task<VCardInfo option>
//    /// Set our vcard
//    abstract SetVCard : VCardInfo -> System.Threading.Tasks.Task
 (*
type ServerDialbackPlugin 
    (stanzas : IXmlStanzaService, neg : INegotiationService) =
     
    let setVCard (info:VCardInfo) =
        async {
            let stanza = Parsing.createVCardElement (stanzas.GenerateNextId()) (Some neg.LocalJid) None (VCardAction.Set info)
            let! stanza = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            
            if stanza.AsInterface.IsEmptyIqResult() then
                return ()
            else
                Parsing.handleStanzaErrors stanza |> ignore
                failwith "expected handleStanzaErrors to throw errors!"
        } 
        |> Log.TraceMe

    let requestVCard (target:JabberId option) =
        async {
            let stanza = Parsing.createVCardElement (stanzas.GenerateNextId()) (Some neg.LocalJid) target (VCardAction.Request)
            let! stanza = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            match Parsing.parseContentVCard stanza with
            | VCardAction.Return res ->
                return res
            | _ ->
                return failwith "expected another stanza at this point"
        } 
        |> Log.TraceMe

    interface IVCardService with
        member __.RequestVCard target = requestVCard target |> Async.StartAsTaskImmediate
        member __.SetVCard info = setVCard info |> Async.StartAsTaskImmediate :> System.Threading.Tasks.Task

    interface IXmppPlugin with
        member __.Name = "VCardPlugin"
        member x.PluginService = Service.FromInstance<IVCardService, _> x

namespace Yaaf.Xmpp.VCard.Server

open Yaaf.Logging
open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.VCard

/// Defines the VCard source
type IVCardSource =
    /// Returns a vcard for the given JabberId
    abstract GetVCard : JabberId -> System.Threading.Tasks.Task<VCardInfo option>
    /// Sets a vcard for the given JabberId
    abstract SetVCard : JabberId * VCardInfo -> System.Threading.Tasks.Task

 
type IVCardConfig =
    abstract VCardSource : IVCardSource
type VCardConfig =
    {
        VCardSource : IVCardSource
    } with
    interface IVCardConfig with
        member x.VCardSource = x.VCardSource
    static member OfInterface (x:IVCardConfig) =
        {
            VCardSource = x.VCardSource
        }
    static member Default =
        {
            VCardSource = Unchecked.defaultof<_>
        }

/// Implementation of http://xmpp.org/extensions/xep-0280.html (XEP-0280: Message Carbons).
/// This plugin is a server side plugin.
type VCardServerPlugin 
    (serverApi : IServerApi, 
     neg : INegotiationService, registrar : IPluginManagerRegistrar,
     disco : IDiscoService, config : IVCardConfig, addressing : IAddressingService,
     stanzas : IXmlStanzaService) =
    do if (obj.ReferenceEquals (config.VCardSource, null)) then Configuration.configFail "VCardSource is required!"
    do disco.RegisterFeatureItem (None, { Var = "vcard-temp"})
    
    let handleVCard (vcardStanza:VCardStanza) = 
        async {
            match vcardStanza.Data with
            | VCardAction.Request ->
                let isCurrent = vcardStanza.Header.To.IsNone
                let requestedVCardJid =
                    if isCurrent then neg.RemoteJid.BareJid
                    else
                        assert (addressing.IsLocalStanzaOnServer (serverApi, vcardStanza))
                        // vcard request to a user of the current server
                        vcardStanza.Header.To.Value
                assert (requestedVCardJid.Resource.IsNone)
                let! vcard = config.VCardSource.GetVCard requestedVCardJid |> Task.await
                match vcard with
                | None ->
                    StanzaException.createSimpleErrorStanza (StanzaErrorType.Cancel) (StanzaErrorConditon.ItemNotFound) vcardStanza
                    |> StanzaException.Raise
                | Some info ->
                    let data = VCardAction.Return (Some info)
                    stanzas.QueueStanzaGeneric None (Parsing.createVCardElement (vcardStanza.Header.Id.Value) None (Some neg.RemoteJid) data)

            | VCardAction.Set info ->
                let isCurrent = vcardStanza.Header.To.IsNone
                if not isCurrent then
                    StanzaException.createSimpleErrorStanza (StanzaErrorType.Auth) (StanzaErrorConditon.Forbidden) vcardStanza
                    |> StanzaException.Raise
                else
                    do! config.VCardSource.SetVCard (neg.RemoteJid.BareJid, info) |> Task.awaitPlain
                    stanzas.QueueStanza None (Stanza.createEmptyIqResult (Some neg.RemoteJid) vcardStanza.Header)
            | _ ->
                failwith "unexpected VCardAction"
        }

    let stanzaReceivedPlugin =
        { new IRawStanzaPlugin with
            member __.ReceivePipeline = 
                { Pipeline.empty "VCard Stanza Pipeline"  with
                    HandlerState = 
                        fun info -> 
                            let elem = info.Result.Element
                            if (Parsing.isContentVCard elem && addressing.IsLocalStanzaOnServer (serverApi, elem)) then
                                HandlerState.ExecuteAndHandle
                            else HandlerState.Unhandled
                    Process =
                        fun info -> 
                            async {
                                let stanza = info.Result.Element
                                assert (Parsing.isContentVCard stanza)
                                Log.Info (fun () -> L "handling vcard element.")
                                let vcardStanza = Stanza<_>.Create(stanza, (Parsing.parseContentVCard stanza))
                                do! handleVCard vcardStanza
                            } 
                            |> Async.StartAsTaskImmediate
                } :> IPipeline<_>
        }

    do registrar.RegisterFor<IRawStanzaPlugin> stanzaReceivedPlugin

    interface IXmppPlugin with
        member __.Name = "ImServerMessagePlugin"
        member __.PluginService = Service.None

module XmppSetup =
  let internal SetVCardConfig (setup, config:IVCardConfig) =
      XmppSetup.SetConfig<VCardConfig,_> (setup, config)
  let internal setVCardConfig config setup = SetVCardConfig(setup, config)
  
#if CSHARP_EXTENSIONS
  [<System.Runtime.CompilerServices.Extension>]
#endif
  let AddVCard (setup: ClientSetup, config) = 
      let setupVCard (runtime:XmppRuntime) = 
          let mgr = runtime.PluginManager
          mgr.RegisterPlugin<VCardServerPlugin>()
      setup
      |> setVCardConfig config
      |> XmppSetup.addHelper ignore setupVCard
  let addVCard config setup = AddVCard(setup, config)

namespace Yaaf.Xmpp.VCard.Server

open Yaaf.Xmpp.Server
open Yaaf.Xmpp.VCard

module XmppServerSetup = 

    let addVCardPlugin config setup =
        setup
        |> XmppServerSetup.addToAllStreams (XmppSetup.addVCard config)
        
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddVCardPlugin(setup, config) = addVCardPlugin config setup
    *)