# RNGeneration

## Basic conditions

What we need to generate randomized items/locations/connectors/etc:

* Areas have Connectors
	* implement as one-way
	* two-way might be shortcut for 2 Connectors
* Connectors have Requirements
* Requirements are sets of Collectables (and Settings)
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

* Settings are a thing
	* Settings can be defined in terms of Collectables
	  (These are set outside of any Area: i.e. "runtime")

## Maybe required?

* Win condition
	* Can be defined as Requirements

## NiceToHave

* Randomness Weights to influence which Areas get which Collectables
* Multi-world: where more than one set of Collectables is distributed over more than one set of Areas
* Spoiler log
* Beatable vs. Completeable
