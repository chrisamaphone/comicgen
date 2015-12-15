# Comic Generator

Current prototype spits out a comic in the following format:

<code>
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
</code>

Transition types Moment, Add, Subtract, Meanwhile, RendezVous, and End are
chosen randomly, and the next panel is generated based on it plus the comic
history.
