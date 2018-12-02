set pagination off
set prompt \001\033[0;32m\002(gdb) \001\033[0m\002
python
import itertools
import gdb
from gdb.FrameDecorator import FrameDecorator


def color(color, text):
    return '\x1b[0;{}m{}\x1b[0m'.format(color, text)


class Color:
    RED = 31
    GREEN = 32
    YELLOW = 33
    BLUE = 34
    MAGENTA = 35
    CYAN = 36
    WHITE = 37


class BtDecorator(FrameDecorator):
    def __init__(self, fobj):
        super(BtDecorator, self).__init__(fobj)
        self.fobj = fobj

    def address(self):
        return None

    def filename(self):
        # todo: is this right?
        #result = super(BtDecorator, self).filename()
        result = self.fobj.filename()
        if not result:
            return result
        try:
            s = 'include/boost/'
            start = result.index(s)
            return color(
                Color.MAGENTA, result[start+len('include/'):])
        except ValueError:
            return color(Color.MAGENTA, result)

    def function(self):
        result = self.fobj.function()
        if not result:
            return result
        if not type(result) == str:
            return result
        replacements = {
            'boost::phoenix': 'phx',
            'v2s_mt_posix::': '',
            'boost::proto::tagns_::tag': 'tag',
            'boost::asio': 'asio',
            'std::_Placeholder<1>': '_1',
            'std::_Placeholder<2>': '_2',
            'boost::beast::websocket': 'ws',
            'std::_Bind': 'std::bind',
        }
        for k, v in replacements.items():
            result = result.replace(k, v)
        return result

class BtFilter(object):
    def __init__(self):
        self.name = "Backtrace"
        self.priority = 1
        self.enabled = True
        gdb.frame_filters[self.name] = self

    def filter(self, iterator):
        # Python 2
        if hasattr(itertools, 'imap'):
            return itertools.imap(BtDecorator, iterator)
        else:
            return map(BtDecorator, iterator)


BtFilter()

class PrettyBacktrace(gdb.Command):
    def __init__(self):
        super(PrettyBacktrace, self).__init__("pbt", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        bt = gdb.execute('bt {}'.format(arg), from_tty=True, to_string=True)
        lines = bt.splitlines()
        for l in lines:
            if l.startswith('#'):
                num, rest = l.split(' ', 1)
                print(
                    '{} {}'.format(
                        color(Color.RED, num), rest))
                print('')
            else:
                print(l)


PrettyBacktrace()
end
