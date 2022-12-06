import pytest
import day06

examples = [("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
            ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
            ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
            ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
            ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26),]


@pytest.mark.parametrize('sample, packet, _', examples)
def test_find_start_of_packet(sample, packet, _):
    assert day06.start_of_packet(sample) == packet


@pytest.mark.parametrize('sample, _, packet', examples)
def test_find_start_of_message(sample, _, packet):
    assert day06.start_of_message(sample) == packet


@pytest.mark.parametrize('sample, packet, _', examples)
def test_find_start_of_packet_marker(sample, packet, _):
    assert day06.start_of_marker(sample, 4) == packet


@pytest.mark.parametrize('sample, _, packet', examples)
def test_find_start_of_message_marker(sample, _, packet):
    assert day06.start_of_marker(sample, 14) == packet


if __name__ == '__main__':
    pytest.main()
