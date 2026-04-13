# pathlib

```python
from pathlib import Path

Path(__file__)           # path to this script file (?)
Path(__file__).parent    # path to the directory of this script file (?)
Path.cwd()               # current working directory - where command was called (?)

path = Path()
path.exists("file.ext")
path.resolve()           # resolve

path = Path("~").expanduser()

path = Path.home() / ".sonia.db"
```
