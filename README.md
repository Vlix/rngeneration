# RNGeneration

## Basic conditions

What we need to generate randomized items/locations/connectors/etc:

* Areas have Connectors
	* implement as one-way
	* two-way might be shortcut for 2 Connectors
* Connectors have Requirements
* Requirements are sets of Collectables (and Options)
	* Requirements should be composable for convenience
* Collectables can be of a certain type
* Areas can have Collectables (skills/items)
	* "Item Location" can be defined in terms of an Area
	* Areas can be limited to a type of Collectable
	  (based on certain Settings, probably?)
	* Areas can be limited to one Collectable
	  (in case the Collectable is non-randomizable, but still a Requirement)
* Connectors have Requirements that restrict the passage through that Connector
	* (given -> access to the area -> what limits passage through the Connector)

* "Item Location with Requirement":
	* "Area with Collectable where Connector has the Requirement"

* Options are a thing
	* Options can be defined in terms of Collectables
	  (These are set outside of any Area: i.e. "runtime")

## Maybe required?

* Win condition
	* Can be defined as Requirements

## Nice To Have

* Randomness Weights to influence which Areas
  get which Collectables
* Multi-world: where more than one set of Collectables
  is distributed over more than one set of Areas
* Spoiler log
* Beatable vs. Completeable


# JSON formats

## Start

The start object indicates which Area is the starting area.

```json
{
  "start" : "Area name"
}
```

```yaml
- start: "Area name"
```

## Area with connectors

Any logically enclosed region is considered an "Area".
An "Area" is used as a means of putting requirements on
progression by defining what bars the entrance into the
connecting "Area". (Item Locations are also a sort of "Area")

The Area "connects" to other Areas and "has" Item Locations
because it is conceptually easier to split the kind of Areas.


```yaml
type: area
name: "Area Name"
connects:
  "Other Area": [reqs]
  "Another Area": [reqs]
has:
  "Item Location": [reqs]
  "Another Item": [reqs]
```

## Item Location

An Item Location isn't necessarily "an item".
It is a place a collectable/item/skill/ability can
be randomized to, or a condition that must be satisfied
in order to unlock other areas.
(e.g. "Adult Link" could be an "Item" in the OoTR,
which you would "get" if you gain access to the pedestal
in the Temple of Time)

An Item Location has a name for debugging and logging's
sake, and it "holds" a certain "Item" when not randomized.

```json
{
  "type": "item",
  "name": "Boss Reward",
  "holds": "Heart Container"
}
```

```yaml
type: item
name: "Boss Reward"
holds: "Heart Container"
```

## Requirement

A Requirement defines what is necessary to get
access to an Area or Item. Though, technically,
it bars access through the connector of the area,
since connectors could also be randomized

(`REQ` is used here as any one of: Item name,
Option or defined Requirement)

It can either be:

- `null`: no requirements
- a `boolean`:
    - `true`: no requirements
    - `false`: impossible
- a `string`: a `REQ`
- an `array`: a list of `REQ`'s
    - empty array: no requirements
- an `object`:
    - any `REQ`: `{"choice": []}`
        - Just one of the `REQ`s in the `array` are required
    - an OR `object`: `{"a": --req--, "or": --req--}`
    - an AND `object`: `{"a": --req--, "and": --req--}`

The list of items means all the items in the
list are required together. This is an easier way
than having to chain AND `object`s.

The AND `object` is still helpful, since you
might want to connect objects with each other in
more complex patterns.

The `"choice"` object means one of the `REQ`s in the
`array` is required, instead of all in case the
`"choice"` object is omitted and a regular `array`
is used.

### Requirement Definition

Top level, requirements can be defined, so that
they can be used by their name when defining connectors.

The `"definition"` field contains the requirements
this definition is a shortcut for.

```json
{
  "type": "requirement",
  "name": "can_smash_rocks",
  "definition": {
    "choice": [
      "Bomb bag",
      "Hammer",
      "Goron Mask"
    ]
  }
}
```

```yaml
type: requirement
name: can_smash_rocks
definition:
  choice:
    - "Bomb bag"
    - Hammer
    - "Goron Mask"
```

## Option

This is a requirement (just like an item or defined
requirement), but this is a flag that's set at runtime
and makes the randomization more modular.

```json
{
  "type": "option",
  "name": "L337 TRICK"
}
```

```yaml
type: option
name: "L337 TRICK"
```


## Example

```json
{
  "type": "area",
  "name": "Kakariko Village",
  "connects": {
    "Hyrule Field": null,
    "Graveyard": null,
    "Death Mountain Trail": null,
    "Windmill": null,
    "Bottom of the Well": [
        "Song of Storms",
        "Ocarina"
      ]
    }
  },
  "has": {
    "Rooftop Man": {
      "this": "Epic Side Hops",
      "or": ["Adult Link", "Hookshot"]
    },
    "Cuccoo Quest": null
  }
}
```

```yaml
type: area
name: Kakariko Village
connects:
  "Hyrule Field": []
has:
  "Rooftop Man":
    this: "Epic Side Hops"
    or: - "Adult Link"
        - "Hookshot"
```
