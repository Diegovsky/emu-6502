from json import dump
from html.parser import HTMLParser

class Parser(HTMLParser):
    insts: list[None|str] = [None]*256
    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag == 'td':
            attributes = {k: v for (k, v) in attrs if v is not None}
            if 'data-description' not in attributes or attributes.get('class') == 'undef':
                return
            desc = attributes['data-description']
            lines = desc.splitlines()
            num = lines[0][2:].strip()
            op = lines[1].lstrip('Sym.:').strip()
            self.insts[int(num, base=16)] = op


parser = Parser()
with open('tbl.html') as f:
    parser.feed(f.read())
    with open('ops.json', 'w') as o:
        dump(parser.insts, o)

