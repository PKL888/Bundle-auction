from os import environ

SESSION_CONFIGS = [
    dict(
        name='auction_baseline',
        display_name="Double auction - Baseline (Stacking)",
        app_sequence=['bundle_auction'],
        num_demo_participants=4,
        treatment='baseline'
    ),
    dict(
        name='auction_single_package',
        display_name="Double auction - Single Package",
        app_sequence=['bundle_auction'],
        num_demo_participants=4,
        treatment='single_package'
    ),
    dict(
        name='auction_package_menu',
        display_name="Double auction - Package Menu",
        app_sequence=['bundle_auction'],
        num_demo_participants=4,
        treatment='package_menu'
    ),
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=0.01, participation_fee=10.00, doc=""
)

PARTICIPANT_FIELDS = []
SESSION_FIELDS = []

LANGUAGE_CODE = 'en'

REAL_WORLD_CURRENCY_CODE = 'AUD'
USE_POINTS = True

ADMIN_USERNAME = 'admin'

ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """ """

SECRET_KEY = '4638897762590'
