from otree.api import *
from math import ceil
import time
import random

doc = """
Multi-attribute continuous double auction with dynamic marginal costs and values, multiple rounds, and role randomisation. Includes stacking and bundling treatments with either a single package or a menu.
"""

class C(BaseConstants):
    NAME_IN_URL = 'econ_lab'
    PLAYERS_PER_GROUP = None

    TRADING_LENGTH = 10
    WAITING_LENGTH = 5

    NUM_PRACTICE_ROUNDS = 1
    NUM_REAL_ROUNDS = 4
    NUM_ROUNDS = NUM_PRACTICE_ROUNDS + NUM_REAL_ROUNDS
    
    ALL_PRODUCTS = ['Product A', 'Product B', 'Package', 'Package 1', 'Package 2']

    # Heterogeneous cost parameters (alpha, gamma)
    PARAMETERS = [
        (0.32, -0.63),
        (0.32, -0.38),
        (0.61, -0.63),
        (0.61, -0.38)
    ]
    REPETITIONS = int(NUM_REAL_ROUNDS / 4)

    # Fixed benefit parameters
    OMEGA = 10.0
    THETA = 0.2

class Subsession(BaseSubsession):
    alpha = models.FloatField()
    gamma = models.FloatField()
    x_param = models.FloatField()

def creating_session(subsession: Subsession):
    if subsession.round_number == 1:
        real_sequence = C.PARAMETERS * C.REPETITIONS
        random.shuffle(real_sequence)
        practice_sequence = random.choices(C.PARAMETERS, k=C.NUM_PRACTICE_ROUNDS)
        full_sequence = practice_sequence + real_sequence
        
        subsession.session.vars['parameter_sequence'] = full_sequence
        
    current_alpha, current_gamma = subsession.session.vars['parameter_sequence'][subsession.round_number - 1]
    subsession.alpha = current_alpha
    subsession.gamma = current_gamma
    
    numerator = 1 - (current_alpha * current_gamma)
    denominator = (current_alpha**2) - (current_alpha * current_gamma)
    subsession.x_param = float(round(numerator / denominator))

    for group in subsession.get_groups():
        players = group.get_players()
        
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

class Group(BaseGroup):
    start_timestamp = models.FloatField(initial=0.0)

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

    @property
    def underlying_qa(self):
        x = int(self.subsession.x_param)
        return self.trades_A + self.trades_Pkg + (self.trades_Pkg1 * 1.0) + (self.trades_Pkg2 * (1.0 / x))

    @property
    def underlying_qb(self):
        x = int(self.subsession.x_param)
        return self.trades_B + self.trades_Pkg + (self.trades_Pkg1 * (1.0 / x)) + (self.trades_Pkg2 * 1.0)

    @property
    def displayed_qa(self):
        x = int(self.subsession.x_param)
        return self.trades_A + self.trades_Pkg + (self.trades_Pkg1 * x) + (self.trades_Pkg2 * 1)

    @property
    def displayed_qb(self):
        x = int(self.subsession.x_param)
        return self.trades_B + self.trades_Pkg + (self.trades_Pkg1 * 1) + (self.trades_Pkg2 * x)

    def get_tc(self, qa, qb):
        if self.is_buyer: return 0.0
        
        a = self.subsession.alpha
        g = self.subsession.gamma

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
            
            base_mv = C.OMEGA - C.THETA * self.buyer_id
            val_int = m * base_mv - C.THETA * n_buyers * (m * (m - 1) / 2.0)            
            val_frac = f * (base_mv - C.THETA * n_buyers * m)
            
            return val_int + val_frac
            
        return value_sum(qa) + value_sum(qb)

    def evaluate_marginal_change(self, p_type, n_buyers):
        qa = self.underlying_qa
        qb = self.underlying_qb
        x = self.subsession.x_param

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
    is_bid = models.BooleanField()
    product_type = models.StringField()
    price = models.FloatField()
    timestamp = models.FloatField()
    is_active = models.BooleanField(initial=True) 

class Trade(ExtraModel):
    group = models.Link(Group)
    buyer = models.Link(Player)
    seller = models.Link(Player)
    product_type = models.StringField()
    price = models.FloatField()
    buyer_profit = models.FloatField()
    seller_profit = models.FloatField()
    timestamp = models.FloatField()

class Welcome(Page):
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player):
        treatment = player.session.config.get('treatment', 'baseline')
        x_weight = int(player.subsession.x_param) 
        
        return {
            'treatment': treatment,
            'x_weight': x_weight,
            'is_buyer': player.is_buyer
        }

class ReadyToStart(WaitPage):
    @staticmethod
    def after_all_players_arrive(group: Group):
        # Record the exact time the round starts
        group.start_timestamp = time.time()

class Trading(Page):
    timeout_seconds = C.TRADING_LENGTH

    @staticmethod
    def vars_for_template(player):
        treatment = player.session.config.get('treatment', 'baseline')
        x = int(player.subsession.x_param)

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
                        price=new_price, timestamp=time.time(), is_active=True
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
                        
                        Trade.create(
                            group=group, buyer=buyer, seller=seller, product_type=p_type, 
                            price=trade_price, buyer_profit=b_profit, seller_profit=s_profit, timestamp=time.time()
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
        if player.round_number == C.NUM_PRACTICE_ROUNDS:
            return None
        return C.WAITING_LENGTH 
       
    @staticmethod
    def is_displayed(player): 
        return player.round_number < C.NUM_ROUNDS
    
    @staticmethod
    def vars_for_template(player):
        is_practice = player.round_number <= C.NUM_PRACTICE_ROUNDS
        is_transition = player.round_number == C.NUM_PRACTICE_ROUNDS
     
        real_rounds = [p for p in player.in_all_rounds() if p.round_number > C.NUM_PRACTICE_ROUNDS]
        total_profit = sum([p.profit for p in real_rounds])
        
        if is_practice:
            display_round = f"Practice Round {player.round_number}"
        else:
            display_round = f"Round {player.round_number - C.NUM_PRACTICE_ROUNDS}"

        return {
            'is_practice': is_practice,
            'is_transition': is_transition,
            'round_profit': player.profit,
            'total_profit': total_profit,
            'display_round': display_round
        }
    
class FinalResults(Page):
    @staticmethod
    def is_displayed(player): 
        return player.round_number == C.NUM_ROUNDS
    
    @staticmethod
    def vars_for_template(player): 
        real_rounds = [p for p in player.in_all_rounds() if p.round_number > C.NUM_PRACTICE_ROUNDS]
        return {'total_profit': sum([p.profit for p in real_rounds])}

def custom_export(players):
    """
    Exports a single CSV file containing five distinct sections:
    SECTION 1: Player Profit Summary Matrix 
    SECTION 2: Detailed Event Log (Offers & Trades) with Elapsed Time
    SECTION 3: Market Convergence (Sequential Trade Prices per Market)
    SECTION 4: Player Trade Counts & Inventory Outcomes
    SECTION 5: Round-Level Market Summaries (Profits & Total Trade Volumes)
    """
    valid_sessions = {p.session for p in players}

    # =========================================================
    # SECTION 1: PLAYER PROFIT SUMMARY 
    # =========================================================
    yield ['=== SECTION 1: PLAYER PROFIT SUMMARY ===']
    
    profit_headers = [
        'session_code', 'treatment', 'participant_code', 'player_id_in_group', 
        'is_buyer', 'buyer_id', 'seller_type'
    ]
    
    for r in range(1, C.NUM_PRACTICE_ROUNDS + 1):
        profit_headers.append(f'practice_round_{r}_profit')
        
    for r in range(1, C.NUM_REAL_ROUNDS + 1):
        profit_headers.append(f'real_round_{r}_profit')
        
    profit_headers.extend(['total_real_profit', 'total_payoff_AUD'])
    
    yield profit_headers

    participants = list(set(p.participant for p in players if p.session in valid_sessions))
    participants.sort(key=lambda part: (part.session.code, part.get_players()[0].id_in_group))

    for part in participants:
        player_in_rounds = part.get_players()
        player_in_rounds.sort(key=lambda x: x.round_number)
        
        first_p = player_in_rounds[0]
        session = first_p.session
        treatment = session.config.get('treatment', 'baseline')
        
        row = [
            session.code,
            treatment,
            part.code,
            first_p.id_in_group,
            first_p.is_buyer,
            first_p.buyer_id,
            first_p.seller_type
        ]

        practice_profit = 0.0
        real_profit = 0.0

        for p in player_in_rounds:
            row.append(p.profit)
            
            if p.round_number <= C.NUM_PRACTICE_ROUNDS:
                practice_profit += p.profit
            else:
                real_profit += p.profit

        payoff_aud = min(10, ceil(float(part.payoff_plus_participation_fee()) * 2) / 2)

        row.extend([real_profit, payoff_aud])
        yield row

    yield []
    yield []

    # =========================================================
    # SECTION 2: EVENT LOG (Offers & Trades)
    # =========================================================
    yield ['=== SECTION 2: EVENT LOG (OFFERS & TRADES) ===']
    yield [
        'session_code', 'treatment', 'round_number', 'is_practice',
        'alpha', 'gamma', 'x_param', 'group_id', 'event_type',
        'timestamp', 'elapsed_time_seconds', 'product_type', 'price', 
        'is_bid', 'player_id', 'trade_buyer_id', 'trade_seller_id', 
        'trade_buyer_profit', 'trade_seller_profit'
    ]

    event_rows = []

    for o in Order.filter():
        if o.group.session not in valid_sessions:
            continue
        subsession = o.group.subsession
        
        # Calculate elapsed time if start_timestamp exists
        start_time = o.group.field_maybe_none('start_timestamp')
        elapsed = (o.timestamp - start_time) if start_time else 0.0

        event_rows.append([
            o.group.session.code,
            o.group.session.config.get('treatment', 'baseline'),
            subsession.round_number,
            subsession.round_number <= C.NUM_PRACTICE_ROUNDS,
            subsession.alpha, subsession.gamma, subsession.x_param,
            o.group.id_in_subsession, 'Offer', o.timestamp, elapsed,
            o.product_type, o.price, o.is_bid, o.player.id_in_group,
            '', '', '', ''
        ])

    for t in Trade.filter():
        if t.group.session not in valid_sessions:
            continue
        subsession = t.group.subsession
        
        # Calculate elapsed time if start_timestamp exists
        start_time = t.group.field_maybe_none('start_timestamp')
        elapsed = (t.timestamp - start_time) if start_time else 0.0

        event_rows.append([
            t.group.session.code,
            t.group.session.config.get('treatment', 'baseline'),
            subsession.round_number,
            subsession.round_number <= C.NUM_PRACTICE_ROUNDS,
            subsession.alpha, subsession.gamma, subsession.x_param,
            t.group.id_in_subsession, 'Trade', t.timestamp, elapsed,
            t.product_type, t.price, '', '',
            t.buyer.id_in_group, t.seller.id_in_group,
            t.buyer_profit, t.seller_profit
        ])

    # Sort primarily by timestamp (index 9) to maintain chronological order
    event_rows.sort(key=lambda r: (r[0], r[2], r[9]))

    for row in event_rows:
        yield row
        
    yield []
    yield []

    # =========================================================
    # SECTION 3: MARKET CONVERGENCE (Trade Prices)
    # =========================================================
    yield ['=== SECTION 3: MARKET CONVERGENCE (TRADE PRICES) ===']
    yield [
        'session_code', 'treatment', 'round_number', 'is_practice', 
        'product_type', 'ordered_trade_prices'
    ]

    trades_by_market = {}
    
    for t in Trade.filter():
        if t.group.session not in valid_sessions:
            continue
            
        sess_code = t.group.session.code
        treatment = t.group.session.config.get('treatment', 'baseline')
        rnd = t.group.subsession.round_number
        is_prac = rnd <= C.NUM_PRACTICE_ROUNDS
        ptype = t.product_type
        
        key = (sess_code, treatment, rnd, is_prac, ptype)
        if key not in trades_by_market:
            trades_by_market[key] = []
            
        trades_by_market[key].append((t.timestamp, t.price))

    for key in sorted(trades_by_market.keys()):
        sorted_trades = sorted(trades_by_market[key], key=lambda x: x[0])
        prices_str = ", ".join([str(price) for ts, price in sorted_trades])
        
        yield [
            key[0], key[1], key[2], key[3], key[4],
            prices_str
        ]

    yield []
    yield []

    # =========================================================
    # SECTION 4: PLAYER TRADE COUNTS & INVENTORY
    # =========================================================
    yield ['=== SECTION 4: PLAYER TRADE COUNTS & INVENTORY ===']
    yield [
        'session_code', 'treatment', 'round_number', 'is_practice', 
        'participant_code', 'player_id_in_group', 'is_buyer', 'buyer_id', 'seller_type', 
        'alpha', 'gamma', 'x_param',
        'trades_A', 'trades_B', 'trades_Pkg', 'trades_Pkg1', 'trades_Pkg2',
        'underlying_qa', 'underlying_qb', 'displayed_qa', 'displayed_qb'
    ]

    all_players = [p for p in players if p.session in valid_sessions]
    all_players.sort(key=lambda p: (p.session.code, p.round_number, p.id_in_group))
    
    for p in all_players:
        subsession = p.subsession
        yield [
            p.session.code,
            p.session.config.get('treatment', 'baseline'),
            p.round_number,
            p.round_number <= C.NUM_PRACTICE_ROUNDS,
            p.participant.code,
            p.id_in_group,
            p.is_buyer,
            p.buyer_id,
            p.seller_type,
            subsession.alpha,
            subsession.gamma,
            subsession.x_param,
            p.trades_A,
            p.trades_B,
            p.trades_Pkg,
            p.trades_Pkg1,
            p.trades_Pkg2,
            p.underlying_qa,
            p.underlying_qb,
            p.displayed_qa,
            p.displayed_qb
        ]

    yield []
    yield []

    # =========================================================
    # SECTION 5: ROUND-LEVEL MARKET SUMMARIES
    # =========================================================
    yield ['=== SECTION 5: ROUND-LEVEL MARKET SUMMARIES ===']
    yield [
        'session_code', 'treatment', 'round_number', 'is_practice', 'group_id',
        'total_market_buyer_profit', 'total_market_seller_profit',
        'total_trades_A', 'total_trades_B', 'total_trades_Pkg', 
        'total_trades_Pkg1', 'total_trades_Pkg2'
    ]

    # Gather all unique groups directly from the provided players list
    unique_groups = {p.group for p in players if p.session in valid_sessions}
    
    # Sort them chronologically
    valid_groups = sorted(
        list(unique_groups), 
        key=lambda x: (x.session.code, x.round_number, x.id_in_subsession)
    )

    for g in valid_groups:
        group_players = g.get_players()
        buyer_profit = sum(p.profit for p in group_players if p.is_buyer)
        seller_profit = sum(p.profit for p in group_players if not p.is_buyer)
        
        # Directly query the Trade model to accurately count unique transactions
        group_trades = Trade.filter(group=g)
        trades_A = sum(1 for t in group_trades if t.product_type == 'Product A')
        trades_B = sum(1 for t in group_trades if t.product_type == 'Product B')
        trades_Pkg = sum(1 for t in group_trades if t.product_type == 'Package')
        trades_Pkg1 = sum(1 for t in group_trades if t.product_type == 'Package 1')
        trades_Pkg2 = sum(1 for t in group_trades if t.product_type == 'Package 2')
        
        yield [
            g.session.code,
            g.session.config.get('treatment', 'baseline'),
            g.round_number,
            g.round_number <= C.NUM_PRACTICE_ROUNDS,
            g.id_in_subsession,
            buyer_profit,
            seller_profit,
            trades_A,
            trades_B,
            trades_Pkg,
            trades_Pkg1,
            trades_Pkg2
        ]

page_sequence = [Welcome, ReadyToStart, Trading, BetweenRounds, FinalResults]
# page_sequence = [ReadyToStart, Trading, BetweenRounds, FinalResults]