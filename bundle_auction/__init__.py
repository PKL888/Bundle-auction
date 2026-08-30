from otree.api import *
from math import ceil
import time
import random
import uuid

doc = """
Multi-attribute continuous double auction with dynamic marginal costs and values, multiple rounds, and fixed roles. Includes stacking and bundling treatments with either a single package or a menu.
"""

CORRECT_ANSWERS = {
    'q1': 6.0,
    'q2': 6.0,
    'q3': 'No, you keep the same role for the entire experiment.',
    'q4': 'It will be lower than $20 (decreasing value).',
    'q5': 'It will be higher than $5 (increasing cost).',
    'q6': 11.0,
    'q7': 'D) Both A and B are correct.',
    'q8': 'D) Both A and B are correct.',
    'q9': 'Higher than the current highest Bid.',
    'q10': 'Lower than the current lowest Ask.',
    'q11': 30.0
}

QUIZ_HINTS = {
    'q1': 'Hint: Profit is calculated as Value minus Price.',
    'q2': 'Hint: Profit is calculated as Price minus Cost.',
    'q3': 'Hint: You will be randomly assigned a role as either a Buyer or a Seller for the entire experiment.',
    'q4': 'Hint: For Buyers, the first unit in a round is worth the most. Each additional unit of that item is worth less than the previous one.',
    'q5': 'Hint: For Sellers, the first unit in a round costs the least. Producing additional units of the same item becomes progressively more expensive.',
    'q6': 'Hint: Subtracting a negative cost effectively adds to your profit.',
    'q7': 'Hint: Review the Experiment instructions document.',
    'q8': 'Hint: Review the Experiment instructions document.',
    'q9': 'Hint: To become the active market bid, your offer must beat all current bids.',
    'q10': 'Hint: To become the active market ask, your offer must undercut all current asks.',
    'q11': 'Hint: Calculate your earnings using the conversion rate (30 experimental points = $1 AUD) and add the $10 participation fee.'
}

QUIZ_EXPLANATIONS = {
    'q1': 'Correct! 15 - 9 = 6 points.',
    'q2': 'Correct! 10 - 4 = 6 points.',
    'q3': 'Correct! Your role remains fixed for the entire experiment.',
    'q4': 'Correct! Values decrease for each additional unit bought.',
    'q5': 'Correct! Costs increase for each additional unit produced.',
    'q6': 'Correct! 8 - (-3) = 11 points.',
    'q7': 'Correct! You can either submit a Bid or click a seller\'s Ask to execute a trade.',
    'q8': 'Correct! You can either submit an Ask or click a buyer\'s Bid execute a trade.',
    'q9': 'Correct! New bids must be strictly higher than the current highest bid.',
    'q10': 'Correct! New asks must be strictly lower than the current lowest ask.',
    'q11': 'Correct! $10 + 600 points ÷ 30 = $10 + $20 = $30.'
}

class C(BaseConstants):
    NAME_IN_URL = 'econ_lab'
    PLAYERS_PER_GROUP = 8

    TRADING_LENGTH = 180
    WAITING_LENGTH = 30

    NUM_PRACTICE_ROUNDS = 2
    NUM_REAL_ROUNDS = 12
    NUM_ROUNDS = NUM_PRACTICE_ROUNDS + NUM_REAL_ROUNDS
    
    ALL_PRODUCTS = ['Product A', 'Product B', 'Package', 'Package 1', 'Package 2']

    ACTIVE_SESSION_NUMBER = 1

    # Heterogeneous cost parameters (alpha, gamma)
    TREATMENT_PARAMS = {
        'HH': (0.32, -0.63),
        'HL': (0.32, -0.38),
        'LH': (0.61, -0.63),
        'LL': (0.61, -0.38)
    }
    
    REPETITIONS_PER_TREATMENT = 3

    SESSION_ORDERS = {
        1: ['HL', 'LH', 'LL', 'HH'],
        2: ['LL', 'HL', 'HH', 'LH'],
        3: ['LH', 'LL', 'HH', 'HL'],
        4: ['HL', 'HH', 'LL', 'LH']
    }
    
    # Fixed benefit parameters
    OMEGA = 10.0
    THETA = 0.18

class Subsession(BaseSubsession):
    pass

def creating_session(subsession: Subsession):
    if subsession.round_number == 1:
        # Randomly allocate 16 participants across 2 groups of 8
        subsession.group_randomly()
        
        # Fetch configurations
        s1 = int(subsession.session.config.get('session_number', C.ACTIVE_SESSION_NUMBER))
        s2 = int(subsession.session.config.get('session_number_2', s1 + 1))
        
        # Build sequences for both potential groups
        seqs = {}
        for g_idx, s_num in enumerate([s1, s2], start=1):
            base_sequence = C.SESSION_ORDERS.get(s_num, ['HH', 'HL', 'LH', 'LL'])
            real_sequence = []
            for code in base_sequence:
                real_sequence.extend([C.TREATMENT_PARAMS[code]] * C.REPETITIONS_PER_TREATMENT)
            seqs[g_idx] = real_sequence
            
        subsession.session.vars['group_sequences'] = seqs
    else:
        subsession.group_like_round(1)

    for group in subsession.get_groups():
        g_idx = group.id_in_subsession # 1 for Group 1, 2 for Group 2
        
        # Assign parameters at the GROUP level
        if subsession.round_number <= C.NUM_PRACTICE_ROUNDS:
            group.alpha = 0.43
            group.gamma = -0.52
            group.omega = 12.0
            group.theta = 0.19
        else:
            real_idx = subsession.round_number - C.NUM_PRACTICE_ROUNDS - 1
            # Fetch the sequence assigned to this specific group index
            group_seq = subsession.session.vars['group_sequences'].get(g_idx, subsession.session.vars['group_sequences'][1])
            current_alpha, current_gamma = group_seq[real_idx]
            group.alpha = current_alpha
            group.gamma = current_gamma
            group.omega = C.OMEGA
            group.theta = C.THETA

        # Calculate x_param
        numerator = 1 - (group.alpha * group.gamma)
        denominator = (group.alpha**2) - (group.alpha * group.gamma)
        group.x_param = float(round(numerator / denominator))

        # Assign fixed roles in Round 1, copy them in subsequent rounds
        if subsession.round_number == 1:
            players = group.get_players()
            random.shuffle(players)
            midpoint = len(players) // 2
            buyer_count = 1
            seller_count = 0
            
            for i, player in enumerate(players):
                if i < midpoint:
                    player.is_buyer = True
                    player.buyer_id = buyer_count
                    player.seller_type = ""
                    buyer_count += 1
                else:
                    player.is_buyer = False
                    player.buyer_id = 0
                    if seller_count % 2 == 0:
                        player.seller_type = 'A'
                    else:
                        player.seller_type = 'B'
                    seller_count += 1
        else:
            for player in group.get_players():
                past_player = player.in_round(1)
                player.is_buyer = past_player.is_buyer
                player.buyer_id = past_player.buyer_id
                player.seller_type = past_player.seller_type

class Group(BaseGroup):
    start_timestamp = models.FloatField(initial=0.0)
    alpha = models.FloatField()
    gamma = models.FloatField()
    x_param = models.FloatField()
    omega = models.FloatField()
    theta = models.FloatField()

class Player(BasePlayer):
    is_buyer = models.BooleanField()
    buyer_id = models.IntegerField(initial=1)
    seller_type = models.StringField(blank=True)
    profit = models.FloatField(initial=0.0)

    trades_A = models.IntegerField(initial=0)
    trades_B = models.IntegerField(initial=0)
    trades_Pkg = models.IntegerField(initial=0)
    trades_Pkg1 = models.IntegerField(initial=0)
    trades_Pkg2 = models.IntegerField(initial=0)

    # ==========================================================================
    # QUIZ FIELDS
    # ==========================================================================
    q1 = models.FloatField(
        label="If you buy a unit with a Value of 15 points at a Price of 9 points, what is your profit?",
        blank=True
    )
    q2 = models.FloatField(
        label="If you sell a unit with a Cost of 4 points at a Price of 10 points, what is your profit?",
        blank=True
    )
    q3 = models.StringField(
        label="Will your role (Buyer or Seller) change between trading rounds?",
        choices=[
            'Yes, roles change every round.',
            'No, you keep the same role for the entire experiment.'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q4 = models.StringField(
        label="As a Buyer, if the first unit of a product you buy has a value of $20, what happens to the value of the second unit of that same product?",
        choices=[
            'It will be higher than $20 (increasing value).',
            'It will be lower than $20 (decreasing value).',
            'It will remain exactly $20 (constant value).'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q5 = models.StringField(
        label="As a Seller, if the first unit of a product you sell costs $5 to produce, what happens to the cost of the second unit of that same product?",
        choices=[
            'It will be higher than $5 (increasing cost).',
            'It will be lower than $5 (decreasing cost).',
            'It will remain exactly $5 (constant cost).'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q6 = models.FloatField(
        label="Suppose a unit displays a negative cost of -$3 points (a production bonus). If you sell this unit for a price of 8 points, what is your profit?",
        blank=True
    )
    q7 = models.StringField(
        label="As a Buyer, how can you execute a trade during a round?",
        choices=[
            'A) Type a Bid into the box and click Submit Bid.',
            'B) Click the Buy button next to a Seller\'s Ask to instantly buy at that price.',
            'C) Neither A nor B are correct.',
            'D) Both A and B are correct.'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q8 = models.StringField(
        label="As a Seller, how can you execute a trade during a round?",
        choices=[
            'A) Type an Ask into the box and click Submit Ask.',
            'B) Click the Sell button next to a Buyer\'s Bid to instantly sell at that price.',
            'C) Neither A nor B are correct.',
            'D) Both A and B are correct.'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q9 = models.StringField(
        label="As a Buyer, to submit a new Bid (buy offer) that appears on the market, your offer must be:",
        choices=[
            'Higher than the current highest Bid.',
            'Lower than the current lowest Bid.',
            'Equal to your personal value.'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q10 = models.StringField(
        label="As a Seller, to submit a new Ask (sell offer) that appears on the market, your offer must be:",
        choices=[
            'Higher than the current highest Ask.',
            'Lower than the current lowest Ask.',
            'Equal to your production cost.'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    q11 = models.FloatField(
        label="If you earn 600 experimental points across the twelve trading rounds, what is your total payout in AUD (including your $10 participation fee)?",
        blank=True
    )
    # ==========================================================================
    # QUESTIONNAIRE FIELDS
    # ==========================================================================
    risk_preference = models.IntegerField(
        label="How do you see yourself? Are you generally a person who is fully prepared to take risks, or do you try to avoid taking risks? (0 = Not at all willing to take risks, 10 = Very willing to take risks)",
        choices=list(range(11)),
        widget=widgets.RadioSelect
    )
    age = models.IntegerField(
        label="What is your age (in years)?",
        min=17, max=100
    )
    gender = models.StringField(
        label="What is your gender?",
        choices=['Female', 'Male', 'Non-binary', 'Prefer not to say'],
        widget=widgets.RadioSelect
    )
    birth_place = models.StringField(
        label="Where were you born?",
        choices=[
            'Australia', 'New Zealand', 'Other Pacific nation', 'China', 'India',
            'East Asia', 'South-East Asia', 'South Asia', 'Other Asia', 'Europe',
            'United States or Canada', 'Central or South America', 'Africa'
        ],
        widget=widgets.RadioSelect
    )
    years_in_australia = models.StringField(
        label="If you were not born in Australia, how long have you lived in Australia?",
        choices=[
            'Not applicable (born in Australia)',
            'Less than 1 year',
            '1-2 years',
            '2-5 years',
            'More than 5 years'
        ],
        widget=widgets.RadioSelect,
        blank=True
    )
    field_of_study = models.StringField(
        label="What is your main field of study at the University?",
        choices=[
            'Management / Business / Commerce', 'Economics', 'Law', 'Engineering',
            'Information Technology', 'Science / Mathematics', 'Exercise / Sport Science',
            'Medicine / Nursing / Health Sciences', 'Social Sciences', 'Education', 'Arts'
        ],
        widget=widgets.RadioSelect
    )
    prev_experiments = models.StringField(
        label="How many economics experiments have you participated in before this one?",
        choices=[
            'None', '1-2 previous experiments', '3-5 previous experiments', 'More than 5 previous experiments'
        ],
        widget=widgets.RadioSelect
    )
    academic_level = models.StringField(
        label="What is your current year of study?",
        choices=[
            '1st year undergraduate', '2nd year undergraduate', '3rd year undergraduate',
            '4th year undergraduate or above', 'Graduate student'
        ],
        widget=widgets.RadioSelect
    )
    gpa = models.StringField(
        label="What is your cumulative GPA at the University?",
        choices=[
            'Between 6.5 and 7.0', 'Between 6.0 and 6.49', 'Between 5.0 and 5.99',
            'Between 4.0 and 4.99', 'Below 4.0', 'Not applicable (this is my first semester at the University)'
        ],
        widget=widgets.RadioSelect
    )
    comments = models.LongStringField(
        label="Do you have any comments about today's experiment?",
        blank=True
    )

    @property
    def underlying_qa(self):
        x = int(self.group.x_param)
        return self.trades_A + self.trades_Pkg + (self.trades_Pkg1 * 1.0) + (self.trades_Pkg2 * (1.0 / x))

    @property
    def underlying_qb(self):
        x = int(self.group.x_param)
        return self.trades_B + self.trades_Pkg + (self.trades_Pkg1 * (1.0 / x)) + (self.trades_Pkg2 * 1.0)

    @property
    def displayed_qa(self):
        x = int(self.group.x_param)
        return self.trades_A + self.trades_Pkg + (self.trades_Pkg1 * x) + (self.trades_Pkg2 * 1)

    @property
    def displayed_qb(self):
        x = int(self.group.x_param)
        return self.trades_B + self.trades_Pkg + (self.trades_Pkg1 * 1) + (self.trades_Pkg2 * x)

    def get_tc(self, qa, qb):
        if self.is_buyer: return 0.0
        
        a = self.group.alpha
        g = self.group.gamma

        if self.seller_type == 'A':
            cost_a = 0.5 * a * qa**2
            cost_b = 0.5 * (1.0 / a) * qb**2
        else:
            cost_a = 0.5 * (1.0 / a) * qa**2
            cost_b = 0.5 * a * qb**2
            
        interaction = g * qa * qb
        return cost_a + cost_b + interaction

    def get_tv(self, qa, qb, n_buyers):
        if not self.is_buyer: return 0.0
        
        def value_sum(q):
            if q <= 0: return 0.0
            
            m = int(q)
            f = q % 1
            
            omega = self.group.omega
            theta = self.group.theta

            base_mv = omega - theta * self.buyer_id
            val_int = m * base_mv - theta * n_buyers * (m * (m - 1) / 2.0)            
            val_frac = f * (base_mv - theta * n_buyers * m)
            
            return val_int + val_frac
            
        return value_sum(qa) + value_sum(qb)
    
    def evaluate_marginal_change(self, p_type, n_buyers):
        qa = self.underlying_qa
        qb = self.underlying_qb
        x = self.group.x_param

        if p_type == 'Product A': delta_a, delta_b = 1.0, 0.0
        elif p_type == 'Product B': delta_a, delta_b = 0.0, 1.0
        elif p_type == 'Package': delta_a, delta_b = 1.0, 1.0
        elif p_type == 'Package 1': delta_a, delta_b = 1.0, (1.0 / x)
        elif p_type == 'Package 2': delta_a, delta_b = (1.0 / x), 1.0
        else: return 0.0
        
        if self.is_buyer:
            return self.get_tv(qa + delta_a, qb + delta_b, n_buyers) - self.get_tv(qa, qb, n_buyers)
        else:
            return self.get_tc(qa + delta_a, qb + delta_b) - self.get_tc(qa, qb)

class Order(ExtraModel):
    group = models.Link(Group)
    player = models.Link(Player)
    order_id = models.StringField()
    is_bid = models.BooleanField()
    product_type = models.StringField()
    price = models.FloatField()
    timestamp = models.FloatField()
    is_active = models.BooleanField(initial=True) 

class Trade(ExtraModel):
    group = models.Link(Group)
    buyer = models.Link(Player)
    seller = models.Link(Player)
    buyer_order_id = models.StringField()
    seller_order_id = models.StringField()
    product_type = models.StringField()
    price = models.FloatField()
    buyer_profit = models.FloatField()
    seller_profit = models.FloatField()
    timestamp = models.FloatField()

def get_active_quiz_questions(player: Player):
    """Filter quiz questions based on role (Buyer/Seller) and Group Treatment."""
    treatment = player.session.config.get('treatment', 'baseline')
    is_buyer = player.is_buyer

    # Core questions for all participants
    active = ['q1', 'q2', 'q3', 'q4', 'q5', 'q7', 'q8', 'q9', 'q10', 'q11']

    if treatment == 'baseline':
        active.append('q6')

    # Sort numerically by question index
    active.sort(key=lambda x: int(x.replace('q', '')))
    return active

class Welcome(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

class Instructions(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1
    
# class Quiz(Page):
#     form_model = 'player'

#     @staticmethod
#     def is_displayed(player: Player):
#         return player.round_number == 1

#     @staticmethod
#     def get_form_fields(player: Player):
#         # Return ALL active questions so passed questions stay rendered on screen
#         return get_active_quiz_questions(player)

#     @staticmethod
#     def js_vars(player: Player):
#         # Pass status lists to JavaScript for visual styling in the browser
#         return {
#             'passed_questions': player.participant.vars.get('quiz_passed_questions', []),
#             'incorrect_questions': player.participant.vars.get('quiz_incorrect_questions', [])
#         }

#     @staticmethod
#     def vars_for_template(player: Player):
#         active = get_active_quiz_questions(player)
#         passed = player.participant.vars.get('quiz_passed_questions', [])
        
#         return {
#             'total_count': len(active),
#             'completed_count': len(passed),
#             'is_retry': player.participant.vars.get('quiz_has_failed', False), # Dynamically reads failure state
#             'is_review': False # Explicitly set to False for the active quiz
#         }

#     @staticmethod
#     def error_message(player: Player, values):
#         active = get_active_quiz_questions(player)
        
#         # Initialize session trackers on initial submission
#         if 'quiz_passed_questions' not in player.participant.vars:
#             player.participant.vars['quiz_passed_questions'] = []
#         if 'quiz_attempts' not in player.participant.vars:
#             player.participant.vars['quiz_attempts'] = {f'q{i}': 0 for i in range(1, 12)}

#         passed = player.participant.vars['quiz_passed_questions']
#         attempts = player.participant.vars['quiz_attempts']
#         incorrect_list = []

#         for q_name in active:
#             # Skip checking questions that the participant already passed
#             if q_name in passed:
#                 continue

#             attempts[q_name] += 1
#             user_val = values.get(q_name)
#             expected_val = CORRECT_ANSWERS.get(q_name)

#             # Validate numeric inputs and radio choices
#             is_correct = False
#             if user_val is not None and user_val != '':
#                 if isinstance(expected_val, (int, float)):
#                     try:
#                         clean_str = str(user_val).replace('$', '').strip()
#                         is_correct = abs(float(clean_str) - float(expected_val)) < 1e-4
#                     except (ValueError, TypeError):
#                         is_correct = False
#                 else:
#                     is_correct = (user_val == expected_val)

#             if is_correct:
#                 if q_name not in passed:
#                     passed.append(q_name)
#             else:
#                 incorrect_list.append(q_name)

#         player.participant.vars['quiz_passed_questions'] = passed
#         player.participant.vars['quiz_incorrect_questions'] = incorrect_list
#         player.participant.vars['quiz_attempts'] = attempts

#         if len(passed) < len(active):
#             player.participant.vars['quiz_has_failed'] = True
#             return f"You answered {len(incorrect_list)} question(s) incorrectly. Correct responses are locked in green. Please review and retry the highlighted question(s) in red."

#         player.participant.vars['quiz_has_failed'] = False

class Quiz(Page):
    form_model = 'player'

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def get_form_fields(player: Player):
        return get_active_quiz_questions(player)

    @staticmethod
    def js_vars(player: Player):
        active = get_active_quiz_questions(player)
        return {
            'active_questions': active,
            'correct_answers': {q: CORRECT_ANSWERS[q] for q in active},
            'hints': {q: QUIZ_HINTS[q] for q in active},
            'explanations': {q: QUIZ_EXPLANATIONS[q] for q in active},
        }

    @staticmethod
    def error_message(player: Player, values):
        # Final server-side validation to ensure answers were not bypassed
        active = get_active_quiz_questions(player)
        errors = {}
        for q_name in active:
            user_val = values.get(q_name)
            expected_val = CORRECT_ANSWERS.get(q_name)
            is_correct = False
            
            if user_val is not None and user_val != '':
                if isinstance(expected_val, (int, float)):
                    try:
                        clean_str = str(user_val).replace('$', '').strip()
                        is_correct = abs(float(clean_str) - float(expected_val)) < 1e-4
                    except (ValueError, TypeError):
                        pass
                else:
                    is_correct = (user_val == expected_val)

            if not is_correct:
                errors[q_name] = "Invalid submission. Please complete the quiz properly."
        
        if errors:
            return errors
        
class QuizReview(Page):
    template_name = 'bundle_auction/Quiz.html' 
    form_model = 'player'

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == 1

    @staticmethod
    def get_form_fields(player: Player):
        return get_active_quiz_questions(player)

    @staticmethod
    def js_vars(player: Player):
        # Pass all active questions as passed so every field is rendered green & locked
        return {
            'passed_questions': player.participant.vars.get('quiz_passed_questions', []),
            'incorrect_questions': []
        }

    @staticmethod
    def vars_for_template(player: Player):
        active = get_active_quiz_questions(player)
        passed = player.participant.vars.get('quiz_passed_questions', [])
        return {
            'total_count': len(active),
            'completed_count': len(passed),
            'is_retry': False, # Explicitly disable the retry banner on the review page
            'is_review': True  # Triggers the success banner in Quiz.html
        }
    
class Introduction(Page):
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player):
        treatment = player.session.config.get('treatment', 'baseline')
        x_weight = int(player.group.x_param) 
        
        return {
            'treatment': treatment,
            'x_weight': x_weight,
            'is_buyer': player.is_buyer
        }

class ReadyToStart(WaitPage):
    wait_for_all_groups = True

    @staticmethod
    def after_all_players_arrive(subsession: Subsession):
        # Record the exact time the round starts for all groups simultaneously
        for group in subsession.get_groups():
            group.start_timestamp = time.time()

class Trading(Page):
    timeout_seconds = C.TRADING_LENGTH

    @staticmethod
    def vars_for_template(player):
        treatment = player.session.config.get('treatment', 'baseline')
        x = int(player.group.x_param)

        if treatment == 'single_package':
            active_products = [{'id': 'Package', 'safe_id': 'package', 'label': 'Package (1 &times; A, 1 &times; B)'}]
        elif treatment == 'package_menu':
            active_products = [
                {'id': 'Package 1', 'safe_id': 'package-1', 'label': f'Package 1 ({x} &times; A, 1 &times; B)'},
                {'id': 'Package 2', 'safe_id': 'package-2', 'label': f'Package 2 (1 &times; A, {x} &times; B)'}
            ]
        else:
            active_products = [
                {'id': 'Product A', 'safe_id': 'product-a', 'label': 'Product A'},
                {'id': 'Product B', 'safe_id': 'product-b', 'label': 'Product B'}
            ]
            
        is_practice = player.round_number <= C.NUM_PRACTICE_ROUNDS
        if is_practice:
            display_round = f"Practice Round {player.round_number}"
        else:
            display_round = f"Round {player.round_number - C.NUM_PRACTICE_ROUNDS}"
            
        return {
            'active_products': active_products, 
            'treatment': treatment,
            'display_round': display_round
        }    
    @staticmethod
    def js_vars(player):
        treatment = player.session.config.get('treatment', 'baseline')
        
        if treatment == 'single_package':
            active_products = [{'id': 'Package', 'safe_id': 'package', 'label': 'Package'}]
        elif treatment == 'package_menu':
            active_products = [
                {'id': 'Package 1', 'safe_id': 'package-1', 'label': 'Package 1'},
                {'id': 'Package 2', 'safe_id': 'package-2', 'label': 'Package 2'}
            ]
        else:
            active_products = [
                {'id': 'Product A', 'safe_id': 'product-a', 'label': 'Product A'},
                {'id': 'Product B', 'safe_id': 'product-b', 'label': 'Product B'}
            ]
            
        return dict(
            active_products=active_products, 
            is_buyer=player.is_buyer, 
            is_practice=(player.round_number <= C.NUM_PRACTICE_ROUNDS)
        )
    
    @staticmethod
    def live_method(player, data):
        group = player.group
        players = group.get_players()
        n_buyers = sum([1 for p in players if p.is_buyer])

        treatment = player.session.config.get('treatment', 'baseline')
        
        if treatment == 'single_package': valid_products = ['Package']
        elif treatment == 'package_menu': valid_products = ['Package 1', 'Package 2']
        else: valid_products = ['Product A', 'Product B']

        if 'action' in data:
            if data['action'] in ['bid', 'ask']:
                is_bid = (data['action'] == 'bid')
                    
                if is_bid == player.is_buyer:
                    new_price = round(float(data['price']), 2)
                    p_type = data['product_type']
                    
                    if is_bid:
                        current_bids = Order.filter(group=group, product_type=p_type, is_bid=True, is_active=True)
                        if current_bids:
                            max_bid = max([b.price for b in current_bids])
                            if new_price <= max_bid:
                                return {player.id_in_group: {'error': f'Invalid Order: Your bid for {p_type} must be greater than the current maximum bid of ${max_bid}.'}}
                    else:
                        current_asks = Order.filter(group=group, product_type=p_type, is_bid=False, is_active=True)
                        if current_asks:
                            min_ask = min([a.price for a in current_asks])
                            if new_price >= min_ask:
                                return {player.id_in_group: {'error': f'Invalid Order: Your ask for {p_type} must be lower than the current minimum ask of ${min_ask}.'}}

                    previous_orders = Order.filter(group=group, player=player, product_type=p_type, is_active=True)
                    for po in previous_orders:
                        po.is_active = False

                    new_order = Order.create(
                        group=group, player=player, is_bid=is_bid, product_type=p_type,
                        price=new_price, timestamp=time.time(), is_active=True,
                        order_id=uuid.uuid4().hex
                    )
                    
                    opposite_is_bid = not new_order.is_bid
                    candidates = Order.filter(group=group, is_active=True, is_bid=opposite_is_bid, product_type=new_order.product_type)
                    valid_candidates = [c for c in candidates if c.player != player]
                    
                    if new_order.is_bid:
                        matches = [c for c in valid_candidates if c.price <= new_order.price]
                        matches.sort(key=lambda x: (x.price, x.timestamp))
                    else:
                        matches = [c for c in valid_candidates if c.price >= new_order.price]
                        matches.sort(key=lambda x: (-x.price, x.timestamp))
                        
                    if matches:
                        match = matches[0] 
                        trade_price = match.price
                        
                        new_order.is_active = False
                        match.is_active = False
                            
                        buyer = new_order.player if new_order.is_bid else match.player
                        seller = match.player if new_order.is_bid else new_order.player
                        
                        b_profit = buyer.evaluate_marginal_change(p_type, n_buyers) - trade_price
                        s_profit = trade_price - seller.evaluate_marginal_change(p_type, n_buyers)
                        
                        buyer.profit += b_profit
                        seller.profit += s_profit
                        
                        for p_obj in [buyer, seller]:
                            if p_type == 'Product A': p_obj.trades_A += 1
                            elif p_type == 'Product B': p_obj.trades_B += 1
                            elif p_type == 'Package': p_obj.trades_Pkg += 1
                            elif p_type == 'Package 1': p_obj.trades_Pkg1 += 1
                            elif p_type == 'Package 2': p_obj.trades_Pkg2 += 1                        
                        
                        # Identify which order belonged to which role
                        b_order_id = new_order.order_id if new_order.is_bid else match.order_id
                        s_order_id = match.order_id if new_order.is_bid else new_order.order_id

                        Trade.create(
                            group=group, buyer=buyer, seller=seller, 
                            buyer_order_id=b_order_id, seller_order_id=s_order_id, # Log the IDs
                            product_type=p_type, price=trade_price, 
                            buyer_profit=b_profit, seller_profit=s_profit, timestamp=time.time()
                        )

        active_orders = Order.filter(group=group, is_active=True)
        trades = Trade.filter(group=group)
            
        response = {}
        for p in players:
            player_order_book = {ptype: {'bids': [], 'asks': []} for ptype in valid_products}
            
            for o in active_orders:
                if o.product_type in valid_products:
                    order_dict = {'price': o.price, 'is_mine': (o.player == p)}
                    if o.is_bid:
                        player_order_book[o.product_type]['bids'].append(order_dict)
                    else:
                        player_order_book[o.product_type]['asks'].append(order_dict)
                        
            for ptype in valid_products:
                player_order_book[ptype]['bids'].sort(key=lambda x: x['price'], reverse=True)
                player_order_book[ptype]['asks'].sort(key=lambda x: x['price'])
                
            player_trade_list = [{'type': t.product_type, 'price': t.price, 'is_mine': (t.buyer == p or t.seller == p)} for t in trades]
            
            marginals_dict = {}
            for prod in valid_products:
                marginals_dict[prod] = float(p.evaluate_marginal_change(prod, n_buyers))

            response[p.id_in_group] = {
                'order_book': player_order_book,
                'trades': player_trade_list,
                'inventory': {
                    'profit': float(p.profit),
                    'displayed_a': p.displayed_qa,
                    'displayed_b': p.displayed_qb,
                    'trades_A': p.trades_A,
                    'trades_B': p.trades_B,
                    'trades_Pkg': p.trades_Pkg,
                    'trades_Pkg1': p.trades_Pkg1,
                    'trades_Pkg2': p.trades_Pkg2,                },
                'marginals': marginals_dict
            }
        return response

    @staticmethod
    def before_next_page(player, timeout_happened):
        if player.round_number > C.NUM_PRACTICE_ROUNDS:
            player.payoff = player.profit
        else:
            player.payoff = 0

class BetweenRounds(Page):
    @staticmethod
    def get_timeout_seconds(player):
        if player.round_number == C.NUM_PRACTICE_ROUNDS or player.round_number == C.NUM_ROUNDS:
            return None
        return C.WAITING_LENGTH 
       
    @staticmethod
    def is_displayed(player): 
        return player.round_number <= C.NUM_ROUNDS
    
    @staticmethod
    def vars_for_template(player):
        is_practice = player.round_number <= C.NUM_PRACTICE_ROUNDS
        is_transition = player.round_number == C.NUM_PRACTICE_ROUNDS
        is_finished = player.round_number == C.NUM_ROUNDS

        real_rounds = [p for p in player.in_all_rounds() if p.round_number > C.NUM_PRACTICE_ROUNDS]
        total_profit = sum([p.profit for p in real_rounds])
        
        if is_practice:
            display_round = f"Practice Round {player.round_number}"
        else:
            display_round = f"Round {player.round_number - C.NUM_PRACTICE_ROUNDS}"

        return {
            'is_practice': is_practice,
            'is_transition': is_transition,
            'is_finished': is_finished,
            'round_profit': player.profit,
            'total_profit': total_profit,
            'display_round': display_round
        }
    
class Questionnaire(Page):
    form_model = 'player'
    form_fields = [
        'risk_preference', 'age', 'gender', 'birth_place',
        'years_in_australia', 'field_of_study', 'prev_experiments',
        'academic_level', 'gpa', 'comments'
    ]

    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == C.NUM_ROUNDS

class ThankYou(Page):
    @staticmethod
    def is_displayed(player: Player):
        return player.round_number == C.NUM_ROUNDS
    
    @staticmethod
    def vars_for_template(player): 
        real_rounds = [p for p in player.in_all_rounds() if p.round_number > C.NUM_PRACTICE_ROUNDS]
        
        # Calculate the rounded AUD payoff exactly like the export
        exact_payoff = float(player.participant.payoff_plus_participation_fee())
        rounded_payoff = max(10, ceil(exact_payoff * 2) / 2)
        
        return {
            'total_profit': sum([p.profit for p in real_rounds]),
            'rounded_payoff': f"{rounded_payoff:.2f}"
        }

def custom_export(players):
    valid_sessions = {p.session for p in players}

    def get_config_seq(group):
        try:
            s1 = int(group.session.config.get('session_number', C.ACTIVE_SESSION_NUMBER))
        except (ValueError, TypeError):
            s1 = C.ACTIVE_SESSION_NUMBER
            
        try:
            s2 = int(group.session.config.get('session_number_2', s1 + 1))
        except (ValueError, TypeError):
            s2 = s1 + 1
            
        return s1 if group.id_in_subsession == 1 else s2

    # =========================================================
    # SECTION 1: CUMULATIVE PROFIT & PAYOUT SUMMARY
    # =========================================================
    yield [
        'record_type', 'session_code', 'treatment', 'group_id', 'config_session_number', 
        'participant_code', 'player_id_in_group', 'is_buyer', 'buyer_id', 'seller_type',
        'total_real_profit', 'total_payoff_AUD'
    ]

    participants = list(set(p.participant for p in players if p.session in valid_sessions))
    participants.sort(key=lambda part: (part.session.code, part.get_players()[0].id_in_group))

    for part in participants:
        player_in_rounds = part.get_players()
        player_in_rounds.sort(key=lambda x: x.round_number)
        
        first_p = player_in_rounds[0]
        session = first_p.session
        treatment = session.config.get('treatment', 'baseline')
        
        real_profit = sum([p.profit for p in player_in_rounds if p.round_number > C.NUM_PRACTICE_ROUNDS])
        payoff_aud = max(10, ceil(float(part.payoff_plus_participation_fee()) * 2) / 2)

        yield [
            'PROFIT_SUMMARY', session.code, treatment, first_p.group.id_in_subsession, get_config_seq(first_p.group),
            part.code, first_p.id_in_group, first_p.is_buyer, first_p.buyer_id, first_p.seller_type,
            real_profit, payoff_aud
        ]

    # =========================================================
    # SECTION 2: EVENT LOG (Offers & Trades)
    # =========================================================
    yield [
        'record_type', 'session_code', 'treatment', 'group_id', 'config_session_number', 
        'round_number', 'is_practice', 'timestamp', 'elapsed_time_seconds', 'event_type', 
        'order_id', 'trade_buyer_order_id', 'trade_seller_order_id',
        'product_type', 'price', 'is_bid', 'player_id', 'trade_buyer_id', 'trade_seller_id', 
        'trade_buyer_profit', 'trade_seller_profit'
    ]

    event_rows = []

    for o in Order.filter():
        if o.group.session not in valid_sessions:
            continue
        
        start_time = o.group.field_maybe_none('start_timestamp')
        elapsed = (o.timestamp - start_time) if start_time else 0.0

        event_rows.append([
            'EVENT_LOG', o.group.session.code, o.group.session.config.get('treatment', 'baseline'),
            o.group.id_in_subsession, get_config_seq(o.group), o.player.round_number, 
            o.player.round_number <= C.NUM_PRACTICE_ROUNDS, o.timestamp, elapsed, 'Offer',
            o.order_id, '', '', o.product_type, o.price, o.is_bid, o.player.id_in_group, 
            '', '', '', ''
        ])

    for t in Trade.filter():
        if t.group.session not in valid_sessions:
            continue
        
        start_time = t.group.field_maybe_none('start_timestamp')
        elapsed = (t.timestamp - start_time) if start_time else 0.0

        event_rows.append([
            'EVENT_LOG', t.group.session.code, t.group.session.config.get('treatment', 'baseline'),
            t.group.id_in_subsession, get_config_seq(t.group), t.buyer.round_number, 
            t.buyer.round_number <= C.NUM_PRACTICE_ROUNDS, t.timestamp, elapsed, 'Trade',
            '', t.buyer_order_id, t.seller_order_id, t.product_type, t.price, '', '', 
            t.buyer.id_in_group, t.seller.id_in_group, round(t.buyer_profit, 2), round(t.seller_profit, 2)
        ])

    # Sort strict chronological/hierarchical: Config Seq -> Group -> Round -> Timestamp
    event_rows.sort(key=lambda r: (r[4], r[3], r[5], r[7]))

    for row in event_rows:
        yield row

    # =========================================================
    # SECTION 3: MARKET CONVERGENCE (Trade Prices)
    # =========================================================
    yield [
        'record_type', 'session_code', 'treatment', 'group_id', 'config_session_number', 
        'round_number', 'is_practice', 'product_type', 'ordered_trade_prices'
    ]

    trades_by_market = {}
    
    for t in Trade.filter():
        if t.group.session not in valid_sessions:
            continue
            
        sess_code = t.group.session.code
        treatment = t.group.session.config.get('treatment', 'baseline')
        rnd = t.buyer.round_number
        is_prac = rnd <= C.NUM_PRACTICE_ROUNDS
        ptype = t.product_type
        g_id = t.group.id_in_subsession
        c_seq = get_config_seq(t.group)
        
        key = (sess_code, treatment, g_id, c_seq, rnd, is_prac, ptype)
        if key not in trades_by_market:
            trades_by_market[key] = []
            
        trades_by_market[key].append((t.timestamp, t.price))

    for key in sorted(trades_by_market.keys()):
        sorted_trades = sorted(trades_by_market[key], key=lambda x: x[0])
        prices_str = ", ".join([str(price) for ts, price in sorted_trades])
        
        yield [
            'MARKET_CONVERGENCE', key[0], key[1], key[2], key[3], key[4], key[5], key[6],
            prices_str
        ]

    # =========================================================
    # SECTION 4: PLAYER-ROUND PANEL (Inventory & Round Profits)
    # =========================================================
    yield [
        'record_type', 'session_code', 'treatment', 'group_id', 'config_session_number', 
        'round_number', 'is_practice', 'participant_code', 'player_id_in_group', 
        'is_buyer', 'buyer_id', 'seller_type', 'round_profit',
        'trades_A', 'trades_B', 'trades_Pkg', 'trades_Pkg1', 'trades_Pkg2',
        'underlying_qa', 'underlying_qb', 'displayed_qa', 'displayed_qb'
    ]

    all_players = [p for p in players if p.session in valid_sessions]
    # Sort strictly by Config Seq -> Group -> Round -> Player ID
    all_players.sort(key=lambda p: (get_config_seq(p.group), p.group.id_in_subsession, p.round_number, p.id_in_group))
    
    for p in all_players:
        yield [
            'PLAYER_ROUND_PANEL', p.session.code, p.session.config.get('treatment', 'baseline'),
            p.group.id_in_subsession, get_config_seq(p.group), p.round_number, p.round_number <= C.NUM_PRACTICE_ROUNDS,
            p.participant.code, p.id_in_group, p.is_buyer, p.buyer_id, p.seller_type, p.profit,
            p.trades_A, p.trades_B, p.trades_Pkg, p.trades_Pkg1, p.trades_Pkg2,
            p.underlying_qa, p.underlying_qb, p.displayed_qa, p.displayed_qb
        ]

    # =========================================================
    # SECTION 5: ROUND-LEVEL MARKET SUMMARIES (Efficiency Params)
    # =========================================================
    yield [
        'record_type', 'session_code', 'treatment', 'group_id', 'config_session_number', 
        'round_number', 'is_practice', 'alpha', 'gamma', 'omega', 'theta', 'x_param',
        'total_market_buyer_profit', 'total_market_seller_profit', 
        'total_trades_A', 'total_trades_B', 'total_trades_Pkg', 'total_trades_Pkg1', 'total_trades_Pkg2'
    ]

    unique_groups = {p.group for p in players if p.session in valid_sessions}
    valid_groups = sorted(
        list(unique_groups), 
        key=lambda x: (get_config_seq(x), x.id_in_subsession, x.round_number)
    )

    for g in valid_groups:
        group_players = g.get_players()
        buyer_profit = sum(p.profit for p in group_players if p.is_buyer)
        seller_profit = sum(p.profit for p in group_players if not p.is_buyer)
        
        group_trades = Trade.filter(group=g)
        trades_A = sum(1 for t in group_trades if t.product_type == 'Product A')
        trades_B = sum(1 for t in group_trades if t.product_type == 'Product B')
        trades_Pkg = sum(1 for t in group_trades if t.product_type == 'Package')
        trades_Pkg1 = sum(1 for t in group_trades if t.product_type == 'Package 1')
        trades_Pkg2 = sum(1 for t in group_trades if t.product_type == 'Package 2')
        
        yield [
            'MARKET_SUMMARY', g.session.code, g.session.config.get('treatment', 'baseline'),
            g.id_in_subsession, get_config_seq(g), g.round_number, g.round_number <= C.NUM_PRACTICE_ROUNDS,
            g.alpha, g.gamma, g.omega, g.theta, g.x_param,
            buyer_profit, seller_profit, trades_A, trades_B, trades_Pkg, trades_Pkg1, trades_Pkg2
        ]
    
    # =========================================================
    # SECTION 6: QUIZ ATTEMPTS & QUESTIONNAIRE RESPONSES
    # =========================================================
    section6_headers = [
        'record_type', 'session_code', 'treatment', 'group_id', 'config_session_number', 
        'participant_code', 'player_id_in_group', 'is_buyer', 'seller_type'
    ]
    
    for i in range(1, 12):
        section6_headers.append(f'q{i}_attempts')
        
    questionnaire_fields = [
        'risk_preference', 'age', 'gender', 'birth_place',
        'years_in_australia', 'field_of_study', 'prev_experiments',
        'academic_level', 'gpa', 'comments'
    ]
    section6_headers.extend(questionnaire_fields)
    
    yield section6_headers
    
    for part in participants:
        player_in_rounds = part.get_players()
        player_in_rounds.sort(key=lambda x: x.round_number)
        
        first_p = player_in_rounds[0]
        last_p = player_in_rounds[-1]
        session = first_p.session
        treatment = session.config.get('treatment', 'baseline')
        
        row = [
            'QUESTIONNAIRE', session.code, treatment, first_p.group.id_in_subsession, get_config_seq(first_p.group),
            part.code, first_p.id_in_group, first_p.is_buyer, first_p.seller_type
        ]
        
        attempts = part.vars.get('quiz_attempts', {})
        active_questions = get_active_quiz_questions(first_p)
        
        for i in range(1, 12):
            q_name = f'q{i}'
            if q_name in active_questions:
                row.append(attempts.get(q_name, 0))
            else:
                row.append('N/A')
                
        for field in questionnaire_fields:
            row.append(getattr(last_p, field))
            
        yield row

# Full sequence:
page_sequence = [Welcome, Instructions, Quiz, Introduction, ReadyToStart, Trading, BetweenRounds, Questionnaire, ThankYou]

# Test sequence:
# page_sequence = [Introduction, ReadyToStart, Trading, BetweenRounds, ThankYou]