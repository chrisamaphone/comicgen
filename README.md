# Comic Generator

Example output:

      [({elements=[1,2],name="aid"},Add),
      ({elements=[3,1,2],name="monolog"},Subtract),
      ({elements=[3,2],name="carry"},Moment),
      ({elements=[3,2],name="whisper"},Meanwhile),
      ({elements=[],name="blank"},Meanwhile),
      ({elements=[1],name="fall"},RendezVous),
      ({elements=[4,3],name="touch"},Meanwhile),
      ({elements=[1],name="monolog"},RendezVous),
      ({elements=[4,3],name="dialog"},End)]
      : (ComicGen.panel * ComicGen.transition) list

Transition types Moment, Add, Subtract, Meanwhile, RendezVous, and End are
generated as a random sequence, or constrained by a grammar. Frames are
generated to fit each transition type or initial position.

