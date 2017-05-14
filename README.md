# Masterly (backend)

"Masterly" is a side project I'm working on to act as universal task management
software. Not "universal" in the sense that it's supposed to work for everyone,
but "universal" in the sense that it's supposed to handle _all_ tasks for a
given user.  One of the main goals is to integrate with other services as
necessary to funnel them all through the same management system effortlessly, so
there aren't multiple sources of "TODO" items.

## Core Abstractions
The core abstractions I'm shooting for have to do with task management. This is
still a work-in-progress, but the idea is to define a set of core abstractions,
and then have each integration map itself onto those ideas.

### Task
A task is a document that contains information about some action that the user
intends to do.

### Urgency vs. Importance
A task has both an urgency and an importance. Urgency is how time-sensitive the
task is. Importance is how much impact the task has. This lets the system
understand how much it should bug you for a given task. For example:

* For **urgent and important** tasks, you can be automatically reminded to do it
  via push notifications to make sure you don't forget about it.
* For **urgent** but unimportant tasks, the task may just mark itself as "won't do"
  after the deadline passes.
* For not urgent but **important** tasks, you can set up an exponential backoff
  strategy for notifications, and after a while determine if it's importance
  should be downgraded and therefore closed.
* For not urgent or important tasks, they can show up on a separate view
  altogether, automatically deleting themselves after a period of time.

### Scheduling
A task can be scheduled for a future date, in which case it will fade into the
background until it's time to work on it. The user can reduce cognitive load by
being able to focus on only the things that are actionable now.

### Reminders
Reminders allow the user to be notified when something needs to be done. When
this happens is based on urgency and importance, and when something was
scheduled.


## Integrations
Integrations are only partly fleshed out at the moment. At minimum, an
integration needs a way to be notified when the user wants to connect to a given
service, and be able to handle an OAuth2 flow to get the proper keys for
background communication. Once an integration has been created and
authenticated, it is called periodically to do any background work that needs to
be done. In addition, a real-time API may be provided so that the user can
interact with the data through the interface, but it's not clear to me yet how
much of the underlying integration should be exposed to the user.

### Gmail Integration
The first integration I'm working on is Gmail, because it's by far the most
valuable. The integration involves authenticating as me via Oauth, and syncing
_all_ messages to disk for later querying. I want to be able to do queries
against Gmail that aren't representable by their search interface - namely
"group by sender", so I can process through multiple emails from the same source
and avoid context switching on a per-email basis.

## Directory Tree

Here's what the directory structure looks like at the moment. As you can see,
most of the code is currently in the Gmail integration to sync messages to
disk. This is because there's a decent amount of complexity involved in doing
this reasonably efficiently.

Although you'll find some files in `Lib` and `test`, they aren't really used
yet. Right now the code is mostly in a working-prototype phase, just feeling out
what the APIs should be and how they could work.

    |-- app
    |-- |-- DocumentStore.hs
    |-- |-- Integration.hs
    |-- |-- Integrations
    |-- |-- |-- Gmail
    |-- |-- |-- |-- Core.hs
    |-- |-- |-- |-- Http.hs
    |-- |-- |-- |-- JSON
    |-- |-- |-- |-- |-- GmailTokenInfo.hs
    |-- |-- |-- |-- |-- ListHistoryResponse.hs
    |-- |-- |-- |-- |-- ListMessagesResponse.hs
    |-- |-- |-- |-- |-- Message.hs
    |-- |-- |-- |-- |-- MessageLabels.hs
    |-- |-- |-- |-- |-- MessageRef.hs
    |-- |-- |-- |-- |-- RefreshResponse.hs
    |-- |-- |-- |-- MultipartBodyParser.hs
    |-- |-- |-- |-- Process.hs
    |-- |-- |-- |-- Storage.hs
    |-- |-- |-- Gmail.hs
    |-- |-- |-- Secrets.hs
    |-- |-- |-- Secrets.hs.example
    |-- |-- Main.hs
    |-- |-- Webserver.hs
    |-- data
    |-- |-- integrations
    |--     |-- gmail.json
    |-- LICENSE
    |-- masterly-backend.cabal
    |-- README.md
    |-- Setup.hs
    |-- src
    |-- |-- Lib.hs
    |-- stack.yaml
    |-- test
        |-- Spec.hs

## Setup
Right now, the application isn't very useful at all (and this is just the
backend portion, anyway), but here's what needs to be done to run it with Gmail
integration.

1. Implement `Secrets.hs` in the `Integrations` directory. Use the example file
   as a reference, but in a nutshell, you need to get API keys for each
   integration you want to use (in this case, just Gmail API credentials).
1. Run the application and, go through the OAuth2 flow by POSTing the endpoint
   `localhost:8080/api/integrations/gmail`. You'll want to go to the URL in the
   redirect response in a web browser (which is where you grant
   permissions). This will then redirect you back to the webserver to store the
   credentials.
1. Run the application again, and the gmail integration will start
   automatically, and attempt to sync all messages in your mailbox to disk in
   the `data/integrations/gmail` directory.
