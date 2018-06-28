extends RigidBody2D

var speed = 0
var turn_speed = 5
var _max_speed = 500
var _acceleration = 0
var _direction = 0
var _bdirection = 0

func _ready():
  connect("body_enter", self, "_on_body_enter")
  set_process(true)
  set_process_input(true)

func _input(event):
  _direction = 0
  if (event.is_action("ui_left")):
    _direction = -1
    print("Moving left")

  if (event.is_action("ui_right")):
    _direction = 1
    print("Moving right")

  if (event.is_action_released("ui_left") or
        event.is_action_released("ui_right")):
    _direction = 0
    print("Released me")

  print("Direction is: ", _direction)

func _process(delta):
  speed += _acceleration
  speed = min(speed, _max_speed)

  if (speed < _max_speed):
    _acceleration += delta

  if (_bdirection != 0):
    set_pos(get_pos() + Vector2(_bdirection * turn_speed, 0))
  else:
    set_pos(get_pos() + Vector2(_direction * turn_speed, 0))

func _on_body_enter(other):
  if (other.is_in_group("enemy")):
    other.hit_by_player()

  speed = 0
  set_process(false)
