// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.ServerDialback
open System.IO
open NUnit.Framework
open FsUnit
open Swensen.Unquote
open Test.Yaaf.Xmpp.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.Stream
open System.Threading.Tasks
open Yaaf.IO
open Yaaf.TestHelper
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.ServerDialback
open System.Xml.Linq


[<TestFixture>]
type ``Test-Yaaf-Xmpp-ServerDialback-Parsing: Test that parsing works``() as this = 
    inherit MyTestClass()

    let genericTest elemString expected =
        let doc = 
          sprintf 
            "<?xml version='1.0'?><stream:stream xmlns:db='jabber:server:dialback' xmlns='jabber:server' xmlns:stream='http://etherx.jabber.org/streams'>%s</stream:stream>"
            elemString
        let d = XDocument.Parse doc
        let openElem = d.Elements() |> Seq.head
        let elem = openElem.Elements() |> Seq.head
        let isRequest = 
          match expected with
          | ServerDialbackAction.Request _ -> true
          | ServerDialbackAction.DoValidate _ -> false
        test <@ Parsing.isDialbackRequestElement elem = isRequest @>
        test <@ Parsing.isDialbackDoValidateElement elem = not isRequest @>
        test <@ Parsing.isDialbackElement elem @>

        let actual = Parsing.parseDialbackElement elem
        test <@ expected = actual @>
        let newElem = Parsing.createDialbackElement actual
        test <@ (Yaaf.Xml.Core.equalXNodeAdvanced elem newElem).IsEqual @>

    [<Test>]
    member this.``Check that we can parse request validate dialback element``() =
        let elem = "<db:result
          from='capulet.example'
          to='montague.example'>b4835385f37fe2895af6c196b59097b16862406db80559900d96bf6fa7d23df3</db:result>" 
        let expected = ServerDialbackAction.Request (DialbackContent.Validate {
            From = JabberId.Parse "capulet.example"
            To = JabberId.Parse "montague.example"
            Key = "b4835385f37fe2895af6c196b59097b16862406db80559900d96bf6fa7d23df3"})
        genericTest elem expected      
       
    [<Test>]
    member this.``Check that we can parse request invalid dialback element``() =
        let elem = "<db:result
          from='montague.example'
          to='capulet.example'
          type='invalid'/>" 
        let expected = ServerDialbackAction.Request (DialbackContent.ValidateResult {
            From = JabberId.Parse "montague.example"
            To = JabberId.Parse "capulet.example"
            IsValid = false })
        genericTest elem expected      
        
    [<Test>]
    member this.``Check that we can parse request valid dialback element``() =
        let elem = "<db:result
          from='montague.example'
          to='capulet.example'
          type='valid'/>" 
        let expected = ServerDialbackAction.Request (DialbackContent.ValidateResult {
            From = JabberId.Parse "montague.example"
            To = JabberId.Parse "capulet.example"
            IsValid = true })
        genericTest elem expected

    [<Test>]
    member this.``Check that we can parse request error dialback element``() =
        let elem = "<db:result
          from='montague.example'
          to='capulet.example'
          type='error'>
        <error type='cancel'>
          <item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
        </error>
      </db:result>" 
        let expected = ServerDialbackAction.Request (DialbackContent.ValidateError {
            From = JabberId.Parse "montague.example"
            To = JabberId.Parse "capulet.example"
            Error = ServerDialbackError.ItemNotFound })
        genericTest elem expected

    [<Test>]
    member this.``Check that we can parse validate error dialback element``() =
        let elem = "<db:verify
          from='montague.example'
          id='417GAF25'
          to='capulet.example'
          type='error'>
        <error type='cancel'>
          <remote-connection-failed xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>
        </error>
      </db:verify>" 
        let expected = ServerDialbackAction.DoValidate ("417GAF25", DialbackContent.ValidateError {
            From = JabberId.Parse "montague.example"
            To = JabberId.Parse "capulet.example"
            Error = ServerDialbackError.RemoteConnectionFailed })
        genericTest elem expected   