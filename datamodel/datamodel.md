% Marketplace Data Model
% Rich Wallace
% Oct 4, 2018

# in the beginning

life was simple. plugins were

::: incremental

- p1, p2, or "not a plugin"

- could be compatible with multiple applications and versions

- were only available in server installs

:::

# then came OnDemand

::: incremental

- some p2 plugins could be installed OnDemand

- some were installed by default

- some were *only* available in OnDemand

:::

# then came SpeakEasy

::: incremental

- add-ons could be "remote"

- weren't really "installed"

- just a descriptor describing how an external site "hooked" into an application

:::

# then came DataCenter (1.0)

- nothing fancy, just a flag

# then came instruction only

::: incremental

- "not a plugin"

- with special instructions for installing

- other special metadata for HipChat and BitBucket

:::

# data model evolved ad-hoc

::: incremental

- nonsense states possible

- bugs in validation logic

- complex query logic

:::

# data model refactor goals

::: incremental

- make impossible states unrepresentable

- make the data model clearer

- make state transitions easier to follow

- simplify queries

- simplify validation logic

:::

# introducing `App`s

::: incremental

- corresponds to `PluginVersion`

- these correspond to `PluginsThree`, `PluginsTwo` and `PluginsOne`, ..., `NotAPlugin`, and `NotAPlugin`

- each type of app has requirements of their own

:::

# CloudApp

::: incremental

- *must* have a descriptor uploaded

- *should* have a URI for where the descriptor came from (we'd like to make that should a must but we have older Connect apps that don't have them because we didn't always require them)

- *can* have a list of scopes

- *must* specify compatibility with at least one Cloud application (versioning doesn't matter because it's Cloud)

- *must* be free or paid via Atlassian

  - if it's paid via Atlassian, it must have a link to documentation
:::

# ServerApp

::: incremental

- *should* have an installable artifact, either a jar or obr file (again, we'd like make this a must but we didn't always host these artifacts and not all apps have them)

- *must* specify if they are `PluginsOne` or `PluginsTwo` (no more weird plugins three or a plugin that isn't a plugin)

- *must* specify an auto update strategy

- *must* be free, paid via vendor, or paid via Atlassian.

  - if it's paid via Atlassian app, it

    - must have a documentation link

    - can be role based

    - must be compatible with only a single application

  - If it's paid via vendor, it

    - *must* have a documentation link

    - *can* have a link to a vendor site to purchase the app

    - *can* be compatible with one or more applications

  - if it's free, it

    - *can* have a documentation link

    - *can* be compatible with one or more applications

:::

# InstructionalApp

::: incremental

- *can* have one or more installation instructions

- *can* specify an integration type

- *must* specify either a link to download a binary or a link for a customer to learn more

- *must* be either free or paid via vendor. If it's paid via vendor, then it

  - *must* have a documentation link

  - *can* have a link to a vendor site to purchase the app

:::

# WorkflowApp

::: incremental

- *must* have an uploaded artifact, a jwb file

- *must* be either free or paid via vendor. If it's paid via vendor, then it

  - *must* have a documentation link

  - *can* have a link to a vendor site to purchase the app

:::

# PluginVersion status

::: incremental

- can also be public or private

- determines if some fields are required or not

- previoulsy determined `PluginVersion.status`

:::

# modeling status

::: incremental

we parameterize our types with a type constructor

```
object Status {
  type Private[A] = Option[A]
  type Public[A] = A
}
```

- `CloudApp[Private, ...]` vs. `CloudApp[Public, ...]`

:::

# modeling payment type

::: incremental

we also parameterize payment type

```
  object CloudApp {
    // snip
    object Paid {
      final case class Free(doc: Option[URI])
      final case class ViaAtlassian(doc: URI)
    }
  }
```

- `CloudApp[..., Free]` vs. `CloudApp[..., ViaAtlassian]` vs. `CloudApp[..., Free \/ ViaAtlassian]`

:::

# `App` sum type

::: incremental

An `App` can be one of many things

- `CloudApp[Private, CloudApp.Paid.Free \/ CloudApp.Paid.ViaAtlassian]`

- `CloudApp[Public, CloudApp.Paid.Free \/ CloudApp.Paid.ViaAtlassian]`

- `ServerApp[Private, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor \/ ServerApp.Paid.ViaAtlassian]`

- `ServerApp[Public, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor \/ ServerApp.Paid.ViaAtlassian]`

- `DataCenterApp[Private, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor \/ DataCenterApp.Paid.ViaAtlassian]`

- `DataCenterApp[Public, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor \/ DataCenterApp.Paid.ViaAtlassian]`

- `InstructionalApp[Private, InstructionalApp.Paid.Free \/ InstructionalApp.Paid.ViaVendor]`

- `InstructionalApp[Public, InstructionalApp.Paid.Free \/ InstructionalApp.Paid.ViaVendor]`

- `WorkflowApp[Private, WorkflowApp.Paid.Free \/ WorkflowApp.Paid.ViaVendor]`

- `WorkflowApp[Public, WorkflowApp.Paid.Free \/ WorkflowApp.Paid.ViaVendor]`

:::

# introducing `AppListing`

::: incremental

- corresponds to `Plugin`

- groups `App`s

- has different constraints depending on the state

:::

# private `AppListing`s

::: incremental

- *must* contain only private apps and must contain at least one of them

- *can* contain either PvA or non-PvA apps

    - if it contains PvA apps, it *can* have draft pricing for that type of app

- when an app listing contains Server or Data Center apps, it *must* contain compatibility history

:::

# public `AppListing`s

::: incremental

- *must* have a logo

- *must* have a tag line

- *must* have a summary

- *must* contain at least one public app of any type with any payment option

- *can* contain any number of private apps of any type with any payment model

- *can* contain draft pricing for an app type when it contains either a private or public version of that app type

- when an app listing contains Server or Data Center apps, it *must* contain compatibility history

- when an app listing contains a free version of a specific type

    - *can* have PvA versions of that app type submitted for approval

- when an app listing contains a PvA version of a specific type

    - *can* have any number of public apps of that type that are either non-PvA or PvA

:::

# other states of `AppListing`

::: incremental

- Submitted - has all the same invariants as a public app listing

- Ready to launch - has all the same invariants as a public app listing

- Rejected - has all the same invariants as a submitted listing

:::

# representing `PrivateAppListing`s

::: incremental

- optional values are represented as `Status.Private`, i.e. `Option`

- app versions are represented as

:::

# representing `PrivateAppListing` versions

::: incremental

- a collection of private cloud apps that are free

    - `NonEmptyList[CloudApp[Private, CloudApp.Paid.Free]]`

-  or paid via atlassian

    - `NonEmptyList[CloudApp[Private, CloudApp.Paid.PaidViaAtlassian]]`

- together, that is

    - `NonEmptyList[CloudApp[Private, CloudApp.Paid.Free]] \&/ NonEmptyList[CloudApp[Private, CloudApp.Paid.PaidViaAtlassian]]`


:::

# representing `PrivateAppListing` versions

::: incremental

- a collection of private server and/or data center apps that are free, paid via vendor, or paid via atlassian

    - `NonEmptyList[ServerApp[Private, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor]] \&/ NonEmptyList[ServerApp[Private, ServerApp.Paid.ViaAtlassian]]`

    - `NonEmptyList[DataCenterApp[Private, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor]] \&/ NonEmptyList[DataCenterApp[Private, DataCenterApp.Paid.ViaAtlassian]]`

- with a compatibility history, `PluginCompatibilityHistory`

    - `
  ( PluginCompatibilityHistory
  , NonEmptyList[ServerApp[Private, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor]] \&/ NonEmptyList[ServerApp[Private, ServerApp.Paid.ViaAtlassian]] \&/
    NonEmptyList[DataCenterApp[Private, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor]] \&/ NonEmptyList[DataCenterApp[Private, DataCenterApp.Paid.ViaAtlassian]]
  )`

:::

# representing `PrivateAppListing` versions

::: incremental

- instructional apps are easy `NonEmptyList[InstructionalApp[Private, InstructionalApp.Paid]]`

- so are workflow apps `NonEmptyList[WorkflowApp[Private, WorkflowApp.Paid]]`

- when we combine them together we get

:::

# representing `PrivateAppListing` versions

```
NonEmptyList[CloudApp[Private, CloudApp.Paid.Free]] \&/ NonEmptyList[CloudApp[Private, CloudApp.Paid.PaidViaAtlassian]] \&/
  ( PluginCompatibilityHistory
  , NonEmptyList[ServerApp[Private, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor]] \&/ NonEmptyList[ServerApp[Private, ServerApp.Paid.ViaAtlassian]] \&/
    NonEmptyList[DataCenterApp[Private, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor]] \&/ NonEmptyList[DataCenterApp[Private, DataCenterApp.Paid.ViaAtlassian]]
  ) \&/
  NonEmptyList[InstructionalApp[Private, InstructionalApp.Paid]] \&/
  NonEmptyList[WorkflowApp[Private, WorkflowApp.Paid]]
```

# representing `PublicAppListing`s

::: incremental

- required values are represented as `Status.Public`, i.e. `Id[A]`, i.e. `A`

- each app type can be different states

    - zero or more private

    - has a submitted app that is not paid via Atlassian

    - has a submitted app that is paid via Atlassian

    - has approved apps that are not paid via Atlassian

    - has approved apps that are paid via Atlassian

:::

# representing `PublicAppListing` versions

::: incremental

private apps are easy, we just reuse types from `PrivateAppListing`

:::

# representing `PublicAppListing` versions

::: incremental

- submitted, non paid via Atlassian apps are a little more interesting

- they can also have private apps

- those private apps can be non paid via Atlassian or paid via Atlassian

- when we have paid via Atlassian, it can have draft pricing

:::

# representing `PublicAppListing` versions

```
final case class SubmittedNonPvA[F[_[_], _], NonPvA, PvA]
  ( submittedNonPvA: F[Public, NonPvA]
  , privateNonPvA: List[F[Private, NonPvA]]
  , privatePvA: Option[(NonEmptyList[F[Private, PvA]], Option[DraftPricing])]
  ) extends SubmittedApps[F, NonPvA, PvA]
```

# representing `PublicAppListing` versions

::: incremental

- submitted, paid via Atlassian apps

    - can have draft pricing

    - can also have private apps

    - those private apps can be non paid via Atlassian or paid via Atlassian

:::

# representing `PublicAppListing` versions

```
final case class SubmittedPvA[F[_[_], _], NonPvA, PvA]
  ( submittedPvA: NonEmptyList[F[Public, PvA]]
  , privatePvA: List[F[Private, PvA]]
  , privateNonPvA: List[F[Private, NonPvA]]
  , draftPricing: Option[DraftPricing]
  ) extends SubmittedApps[F, NonPvA, PvA]
```

# representing `PublicAppListing` versions

introduce some type aliases to make talking about submitted apps easier

```
type SubmittedCloud =
  SubmittedApps[CloudApp, CloudApp.Paid.Free, CloudApp.Paid.ViaAtlassian]

type SubmittedServer =
  SubmittedApps[ServerApp, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor, ServerApp.Paid.ViaAtlassian]

type SubmittedDataCenter =
  SubmittedApps[DataCenterApp, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor, DataCenterApp.Paid.ViaAtlassian]
```

# representing `PublicAppListing` versions

::: incremental

Approved apps have somewhat similar structure

- we have one or more approved, non-PvA app versions

- we have one or more approved, non-PvA app versions, and some that have been submitted for approval to be PvA

- we have one or more approved PvA app versions

:::

# representing `PublicAppListing` versions

we have one or more approved, non-PvA app versions

```
final case class ApprovedNonPvA[F[_[_], _], NonPvA, PvA]
  ( approvedNonPvA: NonEmptyList[F[Public, NonPvA]]
  , privateNonPvA: List[F[Private, NonPvA]]
  , privatePvA: Option[(Option[DraftPricing], NonEmptyList[F[Private, PvA]])]
  ) extends ApprovedApps[F, NonPvA, PvA]
```

# representing `PublicAppListing` versions

we have one or more approved, non-PvA app versions, and some that have been submitted for approval to be PvA

```
final case class ApprovedNonPvAWithSubmittedPvA[F[_[_], _], NonPvA, PvA]
  ( approvedNonPvA: Approved[F, NonPvA]
  , submittedPvA: Submitted[F, PvA]
  , privateNonPvA: List[F[Private, NonPvA]]
  , privatePvA: List[F[Private, PvA]]
  , draftPricing: Option[DraftPricing]
  ) extends ApprovedApps[F, NonPvA, PvA]
```

# representing `PublicAppListing` versions

we have one or more approved PvA app versions

```
final case class ApprovedPvA[F[_[_], _], NonPvA, PvA]
  ( approvedPvA: Approved[F, PvA]
  , privatePvA: List[F[Private, PvA]]
  , approvedNonPvA: List[F[Public, NonPvA]]
  , privateNonPvA: List[F[Private, NonPvA]]
  , draftPricing: Option[DraftPricing]
  ) extends ApprovedApps[F, NonPvA, PvA]
```

# representing `PublicAppListing` versions

more type aliases for great good

```
type ApprovedCloud =
  ApprovedApps[CloudApp, CloudApp.Paid.Free, CloudApp.Paid.ViaAtlassian]

type ApprovedServer =
  ApprovedApps[ServerApp, ServerApp.Paid.Free \/ ServerApp.Paid.ViaVendor, ServerApp.Paid.ViaAtlassian]

type ApprovedDataCenter =
  ApprovedApps[DataCenterApp, DataCenterApp.Paid.Free \/ DataCenterApp.Paid.ViaVendor, DataCenterApp.Paid
```

# representing `PublicAppListing` versions

time to combine them all together

# representing `PublicAppListing` versions

::: incremental

```
(SubmittedCloud \/ ApprovedCloud) \&/
  (SubmittedServer \/ ApprovedServer) \&/
  (SubmittedDataCenter \/ ApprovedDataCenter)
```

- not good enough

- allows us to have no approved apps

:::

# introducing `TheseRights`

```
sealed trait TheseRights[A, B, C, D]
final case class ThisRight[A, B, C, D](b: B, c: Option[C]) extends TheseRights[A, B, C, D]
final case class ThatRight[A, B, C, D](a: Option[A], d: D) extends TheseRights[A, B, C, D]
final case class BothRight[A, B, C, D](b: B, d: D) extends TheseRights[A, B, C, D]
```

# representing `PublicAppListing` versions

- now we can combine submitted and approved apps

- we can guarantee that at least one is non-empty

# representing `PublicAppListing` versions

combine server and datacenter

```
type ServerDataCenter =
  ( PluginCompatibilityHistory,

    TheseRights[
      SubmittedServer,
      ApprovedServer,
      SubmittedDataCenter,
      ApprovedDataCenter
    ]
  )
```

# representing `PublicAppListing` versions

combine those with cloud

```
type CloudServerDataCenter =
  TheseRights[
    SubmittedCloud \/ PrivateCloud,
    ApprovedCloud,
    PrivateServerDataCenter,
    ServerDataCenter
  ]
```

# representing `PublicAppListing` versions

throw in some instructional and workflow goodness

```
type PrivateInstructionalWorkflow =
  NonEmptyList[
    InstructionalApp[Private, InstructionalApp.Paid]] \&/
    NonEmptyList[WorkflowApp[Private, WorkflowApp.Paid]
  ]

type PublicInstructionalWorkflow =
  ( NonEmptyList[InstructionalApp[Public, InstructionalApp.Paid]] \&/ NonEmptyList[WorkflowApp[Public, WorkflowApp.Paid]]
  , privateInstructional: List[InstructionalApp[Private, InstructionalApp.Paid]]
  , privateWorkflow: List[WorkflowApp[Private, WorkflowApp.Paid]]
  )
```

# representing `PublicAppListing` versions

put it all together

```
type Versions =
  TheseRights[
    PrivateInstructionalWorkflow,
    PublicInstructionalWorkflow,
    PrivateServerDataCenter \&/ PrivateCloud,
    CloudServerDataCenter
  ]
```

# representing `PublicAppListing`s

```
type AppListing =
  PrivateAppListing \/
  SubmittedAppListing \/
  ReadyToLaunchAppListing \/
  PublicAppListing \/
  RejectedAppListing
```
